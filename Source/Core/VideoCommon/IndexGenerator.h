// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

// This is currently only used by the DX backend, but it may make sense to
// use it in the GL backend or a future DX10 backend too.

#ifndef _INDEXGENERATOR_H
#define _INDEXGENERATOR_H
#include "CommonTypes.h"

typedef u32 IndexType;

class IndexGenerator
{
public:
	// Init
	static void Init();
	static void Start(IndexType *Indexptr);

	static void AddIndices(int primitive, u32 numVertices);

	// returns numprimitives
	static u32 GetNumVerts() {return base_index;}

	static u32 GetIndexLen() {return (u32)(index_buffer_current - BASEIptr);}

	static u32 GetRemainingIndices();

private:
	// Triangles
	template <bool pr> static IndexType* AddList(IndexType *Iptr, u32 numVerts, u32 index);
	template <bool pr> static IndexType* AddStrip(IndexType *Iptr, u32 numVerts, u32 index);
	template <bool pr> static IndexType* AddFan(IndexType *Iptr, u32 numVerts, u32 index);
	template <bool pr> static IndexType* AddQuads(IndexType *Iptr, u32 numVerts, u32 index);

	// Lines
	static IndexType* AddLineList(IndexType *Iptr, u32 numVerts, u32 index);
	static IndexType* AddLineStrip(IndexType *Iptr, u32 numVerts, u32 index);

	// Points
	static IndexType* AddPoints(IndexType *Iptr, u32 numVerts, u32 index);

	template <bool pr> static IndexType* WriteTriangle(IndexType *Iptr, u32 index1, u32 index2, u32 index3);

	static IndexType *index_buffer_current;
	static IndexType *BASEIptr;
	static u32 base_index;
};

#endif  // _INDEXGENERATOR_H
