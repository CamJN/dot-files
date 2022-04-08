class Libipt < Formula
  desc "an Intel(R) Processor Trace decoder library"
  homepage "https://github.com/intel/libipt/"
  url "https://github.com/intel/libipt/archive/refs/tags/v2.0.5.tar.gz"
  sha256 "95acf499fdf0a0f5ebd07587bb443c702b1fd79f7d869749824234388b9bff80"
  license "BSD-3-Clause"

  depends_on "cmake" => :build
  depends_on "libxed"

  def install
    system "cmake", "-S", ".", "-B", "build","-D","PTDUMP=ON","-D","PTXED=ON", "-D","XED_INCLUDE=#{Formula['libxed'].include}/xed", "-D","XED_LIBDIR=#{Formula['libxed'].lib}", *std_cmake_args
    Dir.chdir("build") do
      system "make"
      system "make", "install"
      bin.install Dir["bin/*"]
    end
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! For Homebrew/homebrew-core
    # this will need to be a test that verifies the functionality of the
    # software. Run the test with `brew test libipt`. Options passed
    # to `brew install` such as `--HEAD` also need to be provided to `brew test`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "false"
  end
end
