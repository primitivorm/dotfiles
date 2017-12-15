#https://llvm.org/docs/CMake.html
#https://llvm.org/docs/HowToCrossCompileLLVM.html

cd ~
if [ ! -d ~/src ]; then
    mkdir src
fi
cd src
if [ ! -d ~/src/llvm-mirror ]; then
    mkdir llvm-mirror
fi    
cd ~/src/llvm-mirror
git clone --recursive https://github.com/llvm-mirror/llvm
cd ~/src/llvm-mirror/llvm/tools
git clone --recursive https://github.com/llvm-mirror/clang
cd ~/src/llvm-mirror/llvm/tools/clang/tools
git clone --recursive https://github.com/llvm-mirror/clang-tools-extra
cd ~/src/llvm-mirror/llvm/tools/
git clone --recursive https://github.com/llvm-mirror/lld
#git clone --recursive https://github.com/llvm-mirror/polly
cd ~/src/llvm-mirror/llvm/projects/
git clone --recursive https://github.com/llvm-mirror/compiler-rt
#git clone --recursive https://github.com/llvm-mirror/openmp
git clone --recursive https://github.com/llvm-mirror/libcxx
git clone --recursive https://github.com/llvm-mirror/libcxxabi
#git clone --recursive https://github.com/llvm-mirror/test-suite
cd ~/src/llvm-mirror/
if [ ! -d ~/src/llvm-mirror/build ]; then
    mkdir build
fi
cd ~/src/llvm-mirror/build
CC='clang' CXX='clang++' cmake -G "Unix Makefiles" \
-DLLVM_BUILD_EXAMPLES=ON \
-DCLANG_BUILD_EXAMPLES=ON \
-DCMAKE_INSTALL_PREFIX=/usr/bin \
-DLLVM_TARGETS_TO_BUILD="X86" \ 
-DBUILD_SHARED_LIBS=ON \
-DLLVM_BUILD_LLVM_DYLIB=ON \
-DCMAKE_BUILD_TYPE=Release ../llvm/ 
 
make -j$(nproc)
sudo make install
