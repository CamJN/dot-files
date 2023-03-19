class Fakeapxs < Formula
  desc "Fake apxs for use with macOS versions after Sierra"
  homepage "https://github.com/CamJN/fakeapxs"
  license "Apache-2.0"
  #head "https://github.com/CamJN/fakeapxs.git", branch: "main"
  #sha256 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  head "file:///Users/camdennarzt/Developer/Bash/fakeapxs/", using: :git, tag: 'v0.2'

  uses_from_macos "apache2"

  def install
    inreplace "fakeapxs", "config_vars.mk >", "#{pkgshare}/config_vars.mk >"

    ["apr-config","apu-config","fakeapxs"].each do |file|
      FileUtils.chmod "ugo+x", buildpath/file
      bin.install file
    end
    libexec.mkpath
    libexec.install buildpath/"libtool"
    FileUtils.chmod "ugo+x", libexec/"libtool"

    pkgshare.install "config_vars.mk"
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
