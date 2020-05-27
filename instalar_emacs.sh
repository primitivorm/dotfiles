#########################################################
#install emacs
#########################################################
if [ ! -d ~/src ]; then
    mkdir ~/src
fi

if [ ! -d ~/src/emacs ]; then
	echo "compilando e instalando emacs..."
	cd ~/src
	git clone https://github.com/emacs-mirror/emacs
	cd emacs
	bash configure
	make -j$(nproc)
	sudo make install
fi
