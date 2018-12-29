# Homebrew Formula for VobSub2SRT
# Usage: brew install https://github.com/ruediger/VobSub2SRT/raw/master/vobsub2srt.rb

require 'formula'

class Vobsub2srt < Formula
  head 'git://github.com/ruediger/VobSub2SRT.git', :using => :git
  homepage 'https://github.com/ruediger/VobSub2SRT'

  depends_on 'cmake'
  depends_on 'tesseract'
  depends_on 'ffmpeg'

  def install
    needle = 'set(CMAKE_REQUIRED_INCLUDES ${Tesseract_INCLUDE_DIR})'
    inreplace "CMakeModules/FindTesseract.cmake", needle, needle+"\n"+'set(CMAKE_REQUIRED_FLAGS "-std=c++11")'

    needle = 'set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMakeModules)'
    inreplace "CMakeLists.txt", needle, needle+"\n"+'set(CMAKE_CXX_STANDARD 11)'

    mkdir "build" do
      system "cmake", "..", *std_cmake_args
      system "make", "install"
    end
  end
end
