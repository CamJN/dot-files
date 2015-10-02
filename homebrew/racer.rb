class Racer < Formula
  desc "Rust Code Completion utility"
  homepage "https://github.com/phildawes/racer"
  url "https://github.com/phildawes/racer/archive/v1.0.0.tar.gz"
  sha256 "78895296ed688eeccbaf7745235f0fc503407bfa718f53583a4dcc9e1246b7f5"

  depends_on "rust"

  def install
    system "cargo", "build", "--release"
    bin.mkpath
    bin.install "./target/release/racer"
    (share/"rust_src").mkpath
    resource("rust_source").stage do
      Dir.glob(["src/llvm/*", "src/test/*", "src/librustdoc/*", "src/etc/snapshot.pyc"]) do |f|
        if File.directory?(f)
          rm_r(f)
        else
          File.delete(f)
        end
      end
      (share/"rust_src").install "src"
    end
  end

  resource "rust_source" do
    url "https://static.rust-lang.org/dist/rustc-1.3.0-src.tar.gz"
    sha256 "ea02d7bc9e7de5b8be3fe6b37ea9b2bd823f9a532c8e4c47d02f37f24ffa3126"
  end

  def caveats
    "You must set RUST_SRC_PATH=#{HOMEBREW_PREFIX}/share/rust_src/src/ in your ENV."
  end

  test do
    ENV["RUST_SRC_PATH"] = (share/"rust_src/src")
    system "#{bin}/racer", "complete", "std::io::B"
  end
end
