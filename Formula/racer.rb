class Racer < Formula
  desc "Rust Code Completion utility"
  homepage "https://github.com/phildawes/racer"
  url "https://github.com/racer-rust/racer/archive/2.0.12.tar.gz"
  sha256 "1fa063d90030c200d74efb25b8501bb9a5add7c2e25cbd4976adf7a73bf715cc"
  depends_on "rust"
  revision 1

  def install
    system "cargo", "build", "--release"
    bin.mkpath
    (libexec/"bin").install "./target/release/racer"
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
    (bin/"racer").write_env_script(libexec/"bin/racer", :RUST_SRC_PATH => share/"rust_src/src")
  end

  resource "rust_source" do
    url Formula['rust'].stable.url
    sha256 Formula['rust'].stable.checksum.hexdigest
  end

  test do
    system "#{bin}/racer", "complete", "std::io::B"
  end
end
