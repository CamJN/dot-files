class Mbuild < Formula
  # include Language::Python::Virtualenv

  desc "python-based build system used for building XED"
  homepage "https://intelxed.github.io/mbuild/"
  license "Apache-2.0"
  head "https://github.com/intelxed/mbuild.git", branch: "main"

  uses_from_macos "python"

  def install
    # ENV.deparallelize  # if your formula fails when building in parallel
    #virtualenv_install_with_resources
    #system "python3", *Language::Python.setup_install_args(prefix)
    system "python3", "setup.py", "install", "--prefix", prefix
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! For Homebrew/homebrew-core
    # this will need to be a test that verifies the functionality of the
    # software. Run the test with `brew test mbuild`. Options passed
    # to `brew install` such as `--HEAD` also need to be provided to `brew test`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "false"
  end
end
