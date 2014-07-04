// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <memory>
#include <string>
#include <vector>
#include "VideoCommon/TextureDecoder.h"
#include "VideoCommon/VideoCommon.h"

class HiresTextures
{
public:
	static void Init(const std::string& gameCode);
	static std::unique_ptr<HiresTextures> GetHiresTex(int width, int height, int texformat, const u8* tex, int tex_size, const u8* tlut, int tlut_size);

	virtual ~HiresTextures() {}

	PC_TexFormat format;
	int width;
	int height;
	int maxlevel;

	std::vector<u32> required_size;
	std::vector<u8*> data;
};
