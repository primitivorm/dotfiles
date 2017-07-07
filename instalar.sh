#install packages
echo "instalando paquetes..."
apt-get update -qq -y
apt-get install aptitude build-essential gcc g++ automake clang-format -y
apt-get install git make cmake flex bison clang llvm llvm-dev lldb libc++-dev -y
apt-get install valgrind vim-gnome dos2unix exuberant-ctags -y
apt-get install python python-dev libxml2-dev libxslt-dev -y
apt-get install tmux ack-grep astyle libtool libunistring-dev -y
apt-get install dpkg-dev libgnome-keyring-dev -y
apt-get install python-pip lcov npm ruby-coffee-script nodejs -y
apt-get install default-jre default-jdk indent -y
apt-get install lua5.2 liblua5.2-dev meld -y
apt-get install gnome-system-monitor clisp libgtk-3-dev -y
apt-get install libjansson-dev libcurl4-openssl-dev curl -y
apt-get install libgccjit-5-dev xd mono-xbuild -y
apt-get install konsole dump gwhere gnuplot plotutils -y
apt-get install k3b vlc brasero libdvdcss libdvdread4 libdvdnav4 gnome-disk-utility -y
apt-get install ninja-build uuid-dev libicu-dev icu-devtools libbsd-dev libedit-dev -y 
apt-get install libsqlite3-dev swig libpython-dev libncurses5-dev pkg-config libblocksruntime-dev autoconf systemtap-sdt-dev tzdata -y
apt-get autoremove -qq -y

#install pip packages
pip install --upgrade pip
pip install --user cpp-coveralls --upgrade
pip install flake8
pip install powerline-status
pip install pygments
pip install sphinx sphinx-autobuild

gem install grammars
gem install pry

#########################################################
#install gnome-keyring
#########################################################
cd /usr/share/doc/git/contrib/credential/gnome-keyring
make
