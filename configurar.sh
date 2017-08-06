#########################################################
#copy configurations
#########################################################
echo "configurando sistema..."
cp -fr .dircolors ~/
cp -f .vimrc ~/
cp -f .bashrc ~/
cp -f .bash_aliases ~/
cp -f .gitconfig ~/
cp -f .profile ~/
cp -f .tmux.conf ~/
cp -fr .atom ~/

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
vim -c ":PluginInstall" ~/.vimrc
find . -type f -exec dos2unix {} \;
if [ ! -d ~/.vim/bundle/YouCompleteMe ]; then 
    #compile YouCompleteMe plugin
    echo "compilando YouCompleteMe plugin..."
    cd ~/.vim/bundle/ && git clone https://github.com/Valloric/YouCompleteMe
    cd ~/.vim/bundle/YouCompleteMe/
    git clean -f
    git pull
    git submodule update --recursive --init
    python install.py --clang-completer
fi

#########################################################
#configure tmux
#########################################################
if [ ! -d ~/.tmuxifier ]; then 
    echo "configurando tmuxifier..."
    git clone https://github.com/jimeh/tmuxifier.git ~/.tmuxifier
    cd ~/.tmuxifier/
    find . -type f -exec dos2unix {} \;
    chmod +x init.sh
    ./init.sh
    cp ~/dotfiles/.dev-window.sh ~/
fi
#########################################################
#configure atom
#########################################################
if [ ! -d ~/.atom ]; then 
    echo "configurando atom..."
    bash ./usr/local/bin/atom-update
    cp -r .atom ~/.atom
fi

#########################################################
#install powerline fonts
#########################################################
if [ ! -d ~/src/fonts ]; then 
    cd ~/src
    git clone https://github.com/powerline/fonts.git
    cd fonts
    ./install.sh
    #install .fonts/*
    cp -r ~/dotfiles/.fonts/ ~/.fonts/
    cd ~/.fonts/
    sudo fc-cache -fv
    #install Fira Mono
    sh ~/dotfiles/getFirafonts.sh
fi

#########################################################
#configure powerline shell
#########################################################
#if [ ! -d ~/powerline-shell ]; then 
#    cd ~
#    git clone https://github.com/banga/powerline-shell
#    cd powerline-shell
#    cp config.py.dist config.py
#    python install.py
#    cd ~
#    ln -s ~/powerline-shell/powerline-shell.py
#    #install fonts
#    git clone https://github.com/powerline/fonts
#    cd fonts
#    ./install.sh
#    rm -fr fonts
#fi
