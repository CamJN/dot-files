class Libxed < Formula
  desc "x86 encoder decoder"
  homepage "https://intelxed.github.io/"
  url "https://github.com/intelxed/xed/archive/refs/tags/12.0.1.tar.gz"
  sha256 "b2dfa6c0e92eaefa326892cf85a6b65aa8a4eabaea9953c3c1dc078f24f1b0e5"
  license "Apache-2.0"

  depends_on "mbuild" => :build

  def install
    #git clone https://github.com/intelxed/mbuild.git mbuild

    # ENV.deparallelize  # if your formula fails when building in parallel
    # Remove unrecognized options if warned by configure
    # https://rubydoc.brew.sh/Formula.html#std_configure_args-instance_method

    # --cc=CC               full path to C compiler
    # --cxx=CXX             full path to C++ compiler
    # --linker=LINKER       full path to linker
    # --ar=AR               full path to archiver (lib/ar)
    # --as=AS               full path to assembler (gas/as/ml/ml64)
    # --host-cpu=ARG_HOST_CPU Host CPU, typically ia32, intel64 or x86-64
    # --host-os=ARG_HOST_OS   Host OS (where the binary runs)

    # --opt=OPT                         Optimization level noopt, 0, 1, 2, 3
    # --extra-defines=EXTRA_DEFINES     Extra preprocessor defines
    # --extra-flags=EXTRA_FLAGS         Extra values for CXXFLAGS and CCFLAGS
    # --extra-cxxflags=EXTRA_CXXFLAGS   Extra values for CXXFLAGS
    # --extra-ccflags=EXTRA_CCFLAGS     Extra values for CCFLAGS
    # --extra-linkflags=EXTRA_LINKFLAGS Extra values for LINKFLAGS
    # --extra-libs=EXTRA_LIBS           Extra values for LIBS

    # -j JOBS, --jobs=JOBS  Number of concurrent worker threads to use.
    pyver = Language::Python.major_minor_version "python3"
    ENV['PYTHONPATH'] = Formula['mbuild'].opt_lib/"python#{pyver}/site-packages"
    system "./mfile.py","-v","50", "--shared", "--install-dir=#{prefix}", "--compiler=clang", "install"
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! For Homebrew/homebrew-core
    # this will need to be a test that verifies the functionality of the
    # software. Run the test with `brew test libxed`. Options passed
    # to `brew install` such as `--HEAD` also need to be provided to `brew test`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "false"
  end
end
