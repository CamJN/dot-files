# Alt: https://github.com/rafket/pam_duress
class PamDuress < Formula
  desc "A pam module to run commands in duress situations"
  homepage "https://github.com/nuvious/pam-duress"
  license "LGPL-3.0-or-later"
  head "https://github.com/nuvious/pam-duress.git", branch: "main"

  depends_on "pkg-config" => :build
  depends_on "openssl@3"

  def install
    inreplace "Makefile" do |s|
      s.gsub! '/usr/local', prefix
    end

    system "make", "CC=clang", "PAM_DIR=pam"

    (lib/"pam").mkpath
    (lib/"pam").install "bin/pam_duress.so"
    bin.install "bin/duress_sign"
    bin.install "bin/pam_test"
  end

  def post_install
    (etc/"duress.d").mkpath
    touch (etc/"duress.d/.keepme")
    chmod 0500, (etc/"duress.d")
  end

  test do
    (testpath/"duress").mkpath
    touch (testpath/"duress/delete_workspace.sh")
    # somehow feed password to stdin twice
    system (bin/"duress_sign"), (testpath/"duress/delete_workspace.sh")
    # check hash
  end
end
