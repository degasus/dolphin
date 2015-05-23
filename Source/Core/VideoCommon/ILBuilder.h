// Copyright 2015 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#pragma once

#include <limits>
#include <memory>
#include <string>
#include <typeindex>
#include <vector>

// damn std::max isn't a constexpr
constexpr int const& constexpr_max(int const& a, int const& b)
{
	return a > b ? a : b;
}

/**
 * This is a class to generate a list of operations which shall represent a shader.
 * To generate this shader, a non-SSA form is used within the local scope of the generator.
 * This non-SSA form is templated to validate the type and the vector size at compilation time.
 * It is only used as an API to write clean and safe code. Internally, a SSA value is generated
 * on each overloaded operation. Afterwards, this SSA values can be written as a shader string.
 */
class ILBuilder
{
public:
	// Reset the enable flag to get only no-ops.
	ILBuilder(bool _enabled = true)
	: enabled(_enabled), dummy_value(typeid(void), 0, OP_IMM)
	{
	}

	// Operation how to generate the SSA value
	enum OP {OP_GLOBAL, OP_IMM, OP_CAST, OP_COMBINE, OP_REPEAT, OP_SELECT,
		OP_ADD, OP_MINUS, OP_MUL, OP_DIV,
		OP_MOD, OP_AND, OP_OR, OP_SHL, OP_SHR,
		OP_GT, OP_GE, OP_EQ, OP_LE, OP_LT, OP_NE
	};

	/**
	 * SSA IL values. Do not modify them on your own, only use the wrapper class Value.
	 */
	class _Value {
	public:
		_Value(std::type_index _t, int _components, OP _op)
		: t(_t), components(_components), op(_op)
		{
			for (auto& p : parents)
				p = nullptr;
			for (auto& i : data)
				i = 0;
		}

		// Name within the shader file
		std::string name;

		// Type of this values
		const std::type_index t;

		// Vector width of this values
		const int components;

		// The operation how to create this new value
		OP op;

		// The source of this operation, may be nullptr
		_Value* parents[2];

		// Constant data of this operation, may be casted from any type
		u32 data[4];
	};

	/**
	 * Non-SSA template based value. It just points to the corresponding SSA value.
	 * It uses a bit template magic to validate the type and the size of the vector.
	 */
	template<typename T, int C> class Value
	{
	public:
		Value(ILBuilder* _builder, _Value* _value) : builder(_builder), value(_value) {}

		// Get the name used for this value within the shader.
		// Keep care that every operation which uses this value will also touch this name.
		const std::string& GetShaderName()
		{
			return value->name;
		}

		// Reference to the builder. Needed to create new SSA values.
		ILBuilder* builder;

		// The SSA value itself.
		_Value* value;

		#define COMMA , // There is no way to escape a comma with a macro

		// Helper macro to easily generate small OP-code instructions
		#define operation(Tret, Cret, opcode, name, args, code) \
			Value<Tret, Cret> name(args) \
			{ \
				Value<Tret, Cret> newvalue(builder, builder->createValue<Tret, Cret>(OP_ ## opcode)); \
				_Value& v = *newvalue.value; \
				v.parents[0] = value; \
				do { code } while (0); \
				return newvalue; \
			}

		// Explicit type casting function. There is no implicit type casting at all.
		// my_value.as<u8>()
		template<typename T2> operation(T2, C, CAST, as,, {})

		// Misuse of the comma operator to append vectors
		// my_vector = (my_value1, my_value2, my_value3)
		template<int C2> operation(T, C + C2, COMBINE, operator COMMA, Value<T COMMA C2> v2,
		{
			v.parents[1] = v2.value;
		})

		// Selection of a single element of the vector
		// my_vector[0]
		// TODO: implement a way to select more than one element at the same time
		operation(T, 1, SELECT, operator[], u32 s1,
		{
			v.data[0] = s1;
		})

		// Repeating vectors by a constant times
		// my_vector = my_value.repeat<4>()
		template<int C2> operation(T, C * C2, REPEAT, repeat,,
		{
			v.data[0] = C2;
		})

		/**
		 * Helper functions for "common" operations.
		 * Implicit type convertion isn't allowed.
		 * But there is an implicit repeating on applying a vector and a single element,
		 * it will apply the operation to each element within the vector.
		 *
		 * Updating a value is also allowed, but in this case, only the right
		 * value will be repeated implicitly.
		 *
		 * This helper macro allows to define a static_assert to only provide this operation to some kinds of types.
		 */
		#define _common_operation(opcode, operatorname, assert) \
			template <int C2> operation(T, constexpr_max(C COMMA C2), opcode, operator operatorname, Value<T COMMA C2> v2, \
			{ \
				static_assert(assert, "operation " #operatorname " is not allowed in this configuration"); \
				static_assert(C == C2 || C2 == 1 || C == 1, "operator size must match, or one must be 1"); \
				if      (C2 == C) v.parents[1] = v2.value; \
				else if (C2 == 1) v.parents[1] = v2.repeat<C>().value; \
				else if (C  == 1) \
				{ \
					v.parents[0] = repeat<C2>().value; \
					v.parents[1] = v2.value;\
				} \
			}) \
			template <int C2> Value<T COMMA C> operator operatorname ## = (Value<T COMMA C2> v2) \
			{ \
				static_assert(C2 == C || C2 == 1, "operator size must match, or the second one must be 1"); \
				*this = *this operatorname v2; \
				return *this; \
			}
		#define common_operation(opcode, operatorname) _common_operation(opcode, operatorname, true)
		#define common_int_operation(opcode, operatorname) _common_operation(opcode, operatorname, std::numeric_limits<T>::is_integer)

		// Default operators
		// my_value1 += my_value2 * my_value3
		common_operation(ADD, +)
		common_operation(MINUS, -)
		common_operation(MUL, *)
		common_operation(DIV, /)

		// Integer-only default operatos
		common_int_operation(MOD, %)
		common_int_operation(AND, &)
		common_int_operation(OR, |)
		common_int_operation(SHL, <<)
		common_int_operation(SHR, >>)

		// boolean compare operators
		#define cmp_operation(opcode, operatorname) \
			template <int C2> operation(bool, constexpr_max(C COMMA C2), opcode, operator operatorname, Value<T COMMA C2> v2, \
			{ \
				static_assert(C == C2 || C2 == 1 || C == 1, "operator size must match, or one must be 1"); \
				if      (C2 == C) v.parents[1] = v2.value; \
				else if (C2 == 1) v.parents[1] = v2.repeat<C>().value; \
				else if (C  == 1) \
				{ \
					v.parents[0] = repeat<C2>().value; \
					v.parents[1] = v2.value;\
				} \
			})
		cmp_operation(GT, >)
		cmp_operation(GE, >=)
		cmp_operation(EQ, ==)
		cmp_operation(LE, <=)
		cmp_operation(LT, <)
		cmp_operation(NE, !=)
	};

	// Create an immediate value
	// il.Imm<u8>(0xf)
	template<typename T> Value<T,1> Imm(T value) { T array[1] = {value}; return Imm<T, 1>(array); }
	template<typename T, int C> Value<T,C> Imm(T value[C])
	{
		Value<T,C> v(this, createValue<T,C>(OP_IMM));
		static_assert(C < sizeof(v.value->data) / sizeof(v.value->data[0]), "data storage within _Value is too small");
		for (int i = 0; i < C; i++)
		{
			*(T*)(&v.value->data) = value[i];
		}
		return v;
	}

	// Fetch a global variable. This one must already be predefined in the shader
	// and in a valide state.
	// my_vector = il.Get<u8,4>("colors")
	template<typename T, int C> Value<T,C> Get(const char* name)
	{
		Value<T,C> v(this, createValue<T,C>(OP_GLOBAL));
		if (enabled)
			v.value->name = name;
		return v;
	}

	// Helper function to alloc a new SSA values
	template<typename T, int C> _Value* createValue(OP op)
	{
		if (!enabled)
			return &dummy_value;

		_Value* v = new _Value(typeid(T), C, op);
		m_intermediate.push_back(std::unique_ptr<_Value>(v));
		return v;
	};

	// The storage of the SSA values
	std::vector<std::unique_ptr<_Value>> m_intermediate;

	// If disabled, just always return the pointer to the dummy value.
	const bool enabled;
	_Value dummy_value;
};
