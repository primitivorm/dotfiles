cd ~
if [ ! -d ~/src ]; then
    mkdir src
fi
cd src
if [ ! -d ~/src/llvm-mirror ]; then
    mkdir llvm-mirror
fi    
cd llvm-mirror
git clone --recursive https://github.com/llvm-mirror/llvm
cd llvm-mirror/llvm/tools
git clone --recursive https://github.com/llvm-mirror/clang
cd clang/tools
git clone --recursive https://github.com/llvm-mirror/clang-tools-extra
cd ~/src/llvm-mirror/llvm/tools/
git clone --recursive https://github.com/llvm-mirror/lld
git clone --recursive https://github.com/llvm-mirror/polly
cd ~/src/llvm-mirror/llvm/projects/
git clone --recursive https://github.com/llvm-mirror/compiler-rt
git clone --recursive https://github.com/llvm-mirror/openmp
git clone --recursive https://github.com/llvm-mirror/libcxx
git clone --recursive https://github.com/llvm-mirror/libcxxabi
git clone --recursive https://github.com/llvm-mirror/test-suite
cd ~/src/llvm-mirror/llvm/
if [ ! -d ~/src/llvm-mirror/llvm/build ]; then
    mkdir build
fi
cd build
cmake -G "Unix Makefiles" -DLLVM_BUILD_EXAMPLES=1 -DCLANG_BUILD_EXAMPLES=1 -DCMAKE_BUILD_TYPE=Release ..
