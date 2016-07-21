#install packages
echo "instalando paquetes..."
apt-get update -qq -y
apt-get install build-essential gcc g++ git make cmake flex bison clang -y
apt-get install valgrind vim-gnome dos2unix -y
apt-get install python python-dev libxml2-dev libxslt-dev -y
apt-get install tmux ack-grep astyle -y
apt-get install dpkg-dev -y
apt-get install python-pip lcov coverage -y
apt-get autoremove -qq -y
pip install --user cpp-coveralls --upgrade
