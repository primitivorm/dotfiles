#install apache
sudo apt-get install apache2
sudo systemctl stop apache2.service
sudo systemctl start apache2.service
sudo systemctl enable apache2.service

#install mysql
sudo apt-get install mysql-server mysql-client libmysqlclient-dev
sudo systemctl stop mysql.service
sudo systemctl start mysql.service
sudo systemctl enable mysql.service
sudo mysql_secure_installation

#install php
sudo apt-get install php libapache2-mod-php php-mysql

#create symlink
#mkdir /home/primi/public_html
#cd /var/www/html
#sudo ln -s /home/primi/public_html/Furniture

#remove mysql
#sudo apt-get remove --purge mysql\*
#sudo dpkg -l | grep -i mysql
#sudo apt-get clean
#sudo updatedb
