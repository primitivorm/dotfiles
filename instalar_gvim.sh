#remove some packages
# sudo apt remove vim vim-runtime vim-tiny vim-common vim-gui-common -y
#necesarios para compilar gvim
sudo apt install libncurses5-dev libgnome2-dev libgnomeui-dev \
      libgtk2.0-dev libatk1.0-dev libbonoboui2-dev \
      libcairo2-dev libx11-dev libxpm-dev libxt-dev python-dev \
      ruby-dev mercurial -y

if [ ! -d ~/src ]; then
    mkdir ~/src
fi

#########################################################
#install vim
#########################################################
if [ ! -d ~/src/vim ]; then
    	echo "compilando e instalando vim..."
	cd ~/src
	git clone https://github.com/vim/vim
	cd vim
	hg pull
	hg update
	cd src
	make distclean # if you build Vim before
	autoreconf -i	# optional
	bash configure --with-features=huge \
		    --enable-multibyte \
		    --enable-rubyinterp \
		    --enable-pythoninterp \
		    --with-python-config-dir=/usr/lib/python2.7/config-x86_64-linux-gnu/ \
		    --enable-perlinterp \
		    --enable-luainterp \
		    --enable-gui=gnome2 --enable-cscope

	make -j$(nproc)
	sudo make install
fi

#########################################################
#install vim plugins
#########################################################
if [ ! -d ~/.vim/bundle ]; then
    mkdir ~/.vim/bundle/
fi
echo "instalando vim plugins..."
rm -fr ~/.vim/bundle/Vundle.vim
cd ~/.vim/bundle/ && git clone https://github.com/VundleVim/Vundle.vim
find . -type f -exec dos2unix {} \;
gvim -c ":PluginInstall" ~/.vimrc
find . -type f -exec dos2unix {} \;
if [ ! -d ~/.vim/bundle/YouCompleteMe ]; then
    # compile YouCompleteMe plugin
    echo "compilando YouCompleteMe plugin..."
    cd ~/.vim/bundle/ && git clone https://github.com/Valloric/YouCompleteMe
    cd ~/.vim/bundle/YouCompleteMe/
    git clean -f
    git pull
    git submodule update --recursive --init
    python install.py --clang-completer
fi
