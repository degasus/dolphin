// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include <algorithm>
#include <cstring>
#include <map>
#include <string>
#include <utility>
#include <SOIL/SOIL.h>

#include "Common/CommonPaths.h"
#include "Common/FileSearch.h"
#include "Common/FileUtil.h"
#include "Common/StringUtil.h"

#include "Core/ConfigManager.h"

#include "VideoCommon/HiresTextures.h"
#include "VideoCommon/VideoConfig.h"


static std::map<std::string, std::string> s_texture_map;

class HiresTexturesSOIL : public HiresTextures
{
public:
	~HiresTexturesSOIL() override;
};

HiresTexturesSOIL::~HiresTexturesSOIL()
{
	for (u8* e : data)
	{
		SOIL_free_image_data(e);
	}
}

void HiresTextures::Init(const std::string & gameCode)
{
	s_texture_map.clear();

	CFileSearch::XStringVector Directories;

	std::string szDir = StringFromFormat("%s%s", File::GetUserPath(D_HIRESTEXTURES_IDX).c_str(), gameCode.c_str());
	Directories.push_back(szDir);

	for (u32 i = 0; i < Directories.size(); i++)
	{
		File::FSTEntry FST_Temp;
		File::ScanDirectoryTree(Directories[i], FST_Temp);
		for (auto& entry : FST_Temp.children)
		{
			if (entry.isDirectory)
			{
				bool duplicate = false;

				for (auto& Directory : Directories)
				{
					if (Directory == entry.physicalName)
					{
						duplicate = true;
						break;
					}
				}

				if (!duplicate)
					Directories.push_back(entry.physicalName);
			}
		}
	}

	CFileSearch::XStringVector Extensions = {
		"*.png",
		"*.bmp",
		"*.tga",
		"*.dds",
		"*.jpg" // Why not? Could be useful for large photo-like textures
	};

	CFileSearch FileSearch(Extensions, Directories);
	const CFileSearch::XStringVector& rFilenames = FileSearch.GetFileNames();

	const std::string code = StringFromFormat("%s_", gameCode.c_str());

	if (rFilenames.size() > 0)
	{
		for (auto& rFilename : rFilenames)
		{
			std::string FileName;
			SplitPath(rFilename, nullptr, &FileName, nullptr);

			if (FileName.substr(0, code.length()).compare(code) == 0 && s_texture_map.find(FileName) == s_texture_map.end())
				s_texture_map.insert(std::map<std::string, std::string>::value_type(FileName, rFilename));
		}
	}
}

std::string HiresTextures::GetHiresName(int width, int height, int texformat, const u8* tex, int tex_size, const u8* tlut, int tlut_size)
{
	// Hash for a second time, but here we're free to hash as we want to
	u64 tex_hash = GetHashHiresTexture(tex, tex_size, g_ActiveConfig.iSafeTextureCache_ColorSamples);
	u64 tlut_hash = 0;
	if (tlut && tlut_size)
	{
		tlut_hash = GetHashHiresTexture(tlut, tlut_size, g_ActiveConfig.iSafeTextureCache_ColorSamples);
	}
	u32 hash = tex_hash ^ tlut_hash;

	const char* gamecode = SConfig::GetInstance().m_LocalCoreStartupParameter.m_strUniqueID.c_str();
	return StringFromFormat("%s_%08x_%i", gamecode, hash, texformat);
}

std::unique_ptr<HiresTextures> HiresTextures::GetHiresTex(int width, int height, int texformat, const u8* tex, int tex_size, const u8* tlut, int tlut_size)
{
	if (!g_ActiveConfig.bHiresTextures || s_texture_map.empty())
	{
		return nullptr;
	}

	std::string filename = GetHiresName(width, height, texformat, tex, tex_size, tlut, tlut_size);
	if (s_texture_map.find(filename) == s_texture_map.end())
		return nullptr;

	int channels, newWidth, newHeight;
	const char* file = s_texture_map[filename].c_str();
	u8 *temp = SOIL_load_image(file, &newWidth, &newHeight, &channels, SOIL_LOAD_RGBA);
	if (!temp)
	{
		ERROR_LOG(VIDEO, "Custom texture %s failed to load", file);
		return nullptr;
	}

	std::unique_ptr<HiresTextures> entry(new HiresTexturesSOIL());
	entry->maxlevel = 0;
	entry->width = newWidth;
	entry->height = newHeight;
	entry->format = PC_TEX_FMT_RGBA32;
	entry->data.push_back(temp);
	entry->required_size.push_back(newWidth * newHeight * 4);

	if (newWidth * height != newHeight * width)
		ERROR_LOG(VIDEO, "Invalid custom texture size %dx%d for texture %s. The aspect differs from the native size %dx%d.", newWidth, newHeight, file, width, height);
	if (newWidth % width || newHeight % height)
		WARN_LOG(VIDEO, "Invalid custom texture size %dx%d for texture %s. Please use an integer upscaling factor based on the native size %dx%d.", newWidth, newHeight, file, width, height);

	for (int level = 1; true; level++)
	{
		std::string mip_filename = StringFromFormat("%s_mip%u", filename.c_str(), level);
		if (s_texture_map.find(mip_filename) == s_texture_map.end())
			break;

		file = s_texture_map[mip_filename].c_str();
		temp = SOIL_load_image(file, &newWidth, &newHeight, &channels, SOIL_LOAD_RGBA);
		if (!temp)
		{
			ERROR_LOG(VIDEO, "Custom texture %s failed to load", file);
			break;
		}

		int width_req = (entry->width + (1 << level) - 1) >> level;
		int height_req = (entry->height + (1 << level) - 1) >> level;
		if (newWidth != width_req || newHeight != height_req)
		{
			ERROR_LOG(VIDEO, "Invalid custom texture size %dx%d for texture %s. This mipmap layer _must_ be %dx%d.", newWidth, newHeight, file, width_req, height_req);
			SOIL_free_image_data(temp);
			break;
		}

		entry->data.push_back(temp);
		entry->required_size.push_back(newWidth * newHeight * 4);
		entry->maxlevel++;
	}

	INFO_LOG(VIDEO, "Loading custom texture from %s", s_texture_map[filename].c_str());
	return entry;
}
