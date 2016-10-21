#install packages
echo "instalando paquetes..."
apt-get update -qq -y
apt-get install aptitude build-essential gcc g++ automake clang-format -y
apt-get install git make cmake flex bison clang llvm -y
apt-get install valgrind vim-gnome dos2unix -y
apt-get install python python-dev libxml2-dev libxslt-dev -y
apt-get install tmux ack-grep astyle -y
apt-get install dpkg-dev libgnome-keyring-dev -y
apt-get install python-pip lcov npm ruby-coffee-script nodejs -y
apt-get install default-jre default-jdk -y
apt-get install lua5.2 liblua5.2-dev -y
apt-get install gnome-system-monitor clisp -y
apt-get install libjansson-dev libcurl4-openssl-dev curl -y
apt-get autoremove -qq -y
pip install --user cpp-coveralls --upgrade

gem install grammars
gem install pry

#########################################################
#install gnome-keyring
#########################################################
cd /usr/share/doc/git/contrib/credential/gnome-keyring
make
