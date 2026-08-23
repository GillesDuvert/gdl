#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>
#include <iostream>

#if defined(_WIN32)
#include <windows.h>
#elif defined(__APPLE__)
#include <CoreFoundation/CoreFoundation.h>
#include <CoreText/CoreText.h>
#elif defined(__linux__) || defined(__unix__)
#if (HAVE_FONTCONFIG) 
#include <fontconfig/fontconfig.h>
#endif
#endif

std::string FindFontPath(const std::string& fontName) {
#if defined(_WIN32)
    // Windows: Query Registry for font file name
    HKEY hKey;
    if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, "Software\\Microsoft\\Windows NT\\CurrentVersion\\Fonts", 0, KEY_READ, &hKey) != ERROR_SUCCESS) return "";
    char valueData[MAX_PATH];
    DWORD valueDataSize = sizeof(valueData);
    std::string foundPath = "";
    for (DWORD i = 0; RegEnumValueA(hKey, i, valueData, &valueDataSize, NULL, NULL, (LPBYTE)valueData, &valueDataSize) == ERROR_SUCCESS; i++) {
        if (std::string(valueData).find(fontName) != std::string::npos) {
            char fontDir[MAX_PATH];
            GetWindowsDirectoryA(fontDir, MAX_PATH);
            foundPath = std::string(fontDir) + "\\Fonts\\" + std::string(valueData);
            break;
        }
        valueDataSize = sizeof(valueData);
    }
    RegCloseKey(hKey);
    return foundPath;

#elif defined(__APPLE__)
    // macOS: Use CoreText to query available fonts
    CFStringRef cfName = CFStringCreateWithCString(NULL, fontName.c_str(), kCFStringEncodingUTF8);
    CTFontRef ctFont = CTFontCreateWithName(cfName, 0.0, NULL);
    CFRelease(cfName);
    if (!ctFont) return "";
    CFURLRef url = (CFURLRef)CTFontCopyAttribute(ctFont, kCTFontURLAttribute);
    CFRelease(ctFont);
    if (!url) return "";
    char path[PATH_MAX];
    bool success = CFURLGetFileSystemRepresentation(url, true, (UInt8*)path, sizeof(path));
    CFRelease(url);
    return success ? std::string(path) : "";

#elif defined(__linux__) || defined(__unix__) 
	#if (HAVE_FONTCONFIG) 
    // Linux/BSD: Use Fontconfig
    if (!FcInit()) return "";
    FcConfig* config = FcInitLoadConfigAndFonts();
    FcPattern* pat = FcNameParse((const FcChar8*)fontName.c_str());
    FcDefaultSubstitute(pat);
    FcResult res;
    FcPattern* font = FcFontMatch(config, pat, &res);
    std::string foundPath = "";
    if (font) {
        FcChar8* file = NULL;
        if (FcPatternGetString(font, FC_FILE, 0, &file) == FcResultMatch) foundPath = (char*)file;
        FcPatternDestroy(font);
    }
    FcPatternDestroy(pat);
    return foundPath;
    #else
	return "";
    #endif
#else
    return "";
#endif
}
std::string FindFontPath(char *fontName) {
	return FindFontPath(std::string(fontName));
}
// int main(int argc, char** argv) {
//   std::cerr<<FindFontPath(std::string(argv[1]))<<std::endl;
//   return 0;
// }
