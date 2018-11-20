CREATE DATABASE tienda;
USE tienda;

CREATE USER admin_tienda;

GRANT select, insert, delete, update ON tienda.* TO 'admin_tienda'@'localhost' IDENTIFIED BY 'VS~t*{5^CUg8'

FLUSH PRIVILEGES;

CREATE TABLE `categoria` (
 `id` int(11) NOT NULL AUTO_INCREMENT,
 `nombre` varchar(50) NOT NULL,
 PRIMARY KEY (`id`),
 UNIQUE KEY `nombre` (`nombre`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

INSERT INTO categoria(id, nombre) VALUES (null, 'multiflores');
INSERT INTO categoria(id, nombre) VALUES (null, 'sedaflor');
INSERT INTO categoria(id, nombre) VALUES (null, 'termitos');

--rollback
--DROP TABLE `categoria`;

CREATE TABLE `productos` (
 `id` int(11) NOT NULL AUTO_INCREMENT,
 `categoria_id` int(11) NOT NULL,
 `nombre` varchar(255) DEFAULT NULL,
 `precio` decimal(11,0) NOT NULL,
 `descripcion` varchar(255) DEFAULT NULL,
 `imagen` varchar(128) DEFAULT NULL,
 PRIMARY KEY (`id`),
 UNIQUE KEY `imagen` (`imagen`),
 KEY `fk_productos_categoria0` (`categoria_id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

--rollback
--DROP TABLE `productos`;

--multiflores
SET @categoria_id = (SELECT id FROM categoria WHERE nombre='multiflores');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Honey Peque 35 gr.', 15.00 , NULL, '001chiqui.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Honey pimentero 250 gr.', 38.00 , NULL, '002pimentero250.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Honey jar 400 gr.', 55.00 , NULL, '003jar400.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Honey label panel 550 gr.', 65.00 , NULL, '004label550.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Oso 250 mlgr.', 45.00 , NULL, '005oso.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Oval cajetero 1,050 gr.', 120.00 , NULL, '006cajetero.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Flip top 500 ml.', 80.00 , NULL, '007fliptop.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Tarro  550 ml.', 65.00 , NULL, '008tarro.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Plástico 1 Lt.', 140.00 , NULL, '085litroplas.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Cubeta 27 kg.', 1700.00 , NULL, '010cubeta.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Granola 1 kg.', 110.00 , NULL, '011granola.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Palanqueta redonda 1 pza.', 20.00 , NULL, '012palanqueta.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Polen 1 kg.', 250.00 , NULL, '013polen.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, '<p>Complemento alimenticio</p><p>250 gr.</p>', 85.00 , NULL, '014complemento.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Miel Melipona 20 ml.', 65.00 , NULL, '015melipona.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Dulces de miel/propoleo 1 kg.', 90.00 , NULL, '016dulces1kg.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, '<p>Dulces de miel/propoleo</p><p>50 gr.</p>', 15.00 , NULL, '017dulces.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Popotes 50 pzas.', 80.00 , NULL, '018popotes.jpg');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Paletas propoleo y miel 500 pzas.', 500.00 , NULL, '019paleta.jpg');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Cera 1 kg.', 150.00 , NULL, '020cera.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Jalea Real 20 gr.', 100.00 , NULL, '021jaleachica.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Jalea Real 50 gr.', 250.00 , NULL, '022jaleagrande.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, '<p class="p-f11">Panal de abeja Tam. 16X15X4 cm</p><p class="p-f11">De venta en temporada, pregunte</p><p class="p-f11">por existencias.</p>', 70.00 , NULL, '023panal.png');

--update productos set precio = 140.00 where imagen='085litroplas.png';

--sedaflor
SET @categoria_id = (SELECT id FROM categoria WHERE nombre='sedaflor');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Crema antiedad 60 gr.', 250.00 , NULL, '024atiedad.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Crema hidratante 240 ml.', 60.00 , NULL, '025hidra.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Crema de jalea real 50 gr.', 250.00 , NULL, '026jalea.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Gel contorno de ojos 50 gr.', 250.00 , NULL, '027gel.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Jabón hidratante 120 gr.', 35.00 , NULL, '028hidratante.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Jabón exfoliante 120 gr.', 35.00 , NULL, '029exfoliante.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, '<p>Jabón de miel y pólen</p><p> 120 gr.</p>', 35.00 , NULL, '030mielypolen.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Jabón antibacterial 120 gr.', 35.00 , NULL, '031antibacterial.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Jabón reafirmante 120 gr.', 35.00 , NULL, '032reafirmante.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Jabón de miel y romero 120 gr.', 120.00 , NULL, '033romero.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Shampoo de rosas 450 ml.', 45.00 , NULL, '034rosas.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, '<p>Shampoo de anti-caspa</p><p>450 ml.</p>', 45.00 , NULL, '035anticaspa.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, '<p>Shampoo brillo y suavidad</p><p>450 ml.</p>', 45.00 , NULL, '036shasabila.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, '<p>Pomada dolores musculares</p><p>60 gr.</p>', 40.00 , NULL, '037dolores.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, '<p>Presentaciones mini</p><p>de 30 ml. Consulta precio</p>', 00.00 , NULL, '038minis.png'); /*consulta precio pendiente*/

--update productos set nombre = '<p>Presentaciones mini</p><p>de 30 ml.</p>' where imagen = '038minis.png';

--termitos
SET @categoria_id = (SELECT id FROM categoria WHERE nombre='termitos');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Gotero económico 20 ml.', 22.00 , NULL, '039extractoplas.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Gotero de cristal 20 ml', 37.00 , NULL, '040extracto.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Spray 50 ml.', 60.00 , NULL, '041spray.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Jarabe 240 ml.', 60.00 , NULL, '042jarabe.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Envase 1 Lt.', 380.00 , NULL, '043litro.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Gotero económico 20 ml.', 22.00 , NULL, '044extractoplas.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Gotero de cristal 20 ml', 37.00 , NULL, '045extracto.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Spray 50 ml.', 60.00 , NULL, '046spray.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Jarabe 240 ml.', 60.00 , NULL, '047jarabe.png');
INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, 'Bolsa 1 kg.', 1700.00 , NULL, '048propoleokg.png');

--INSERT INTO productos(id, categoria_id, nombre, precio, descripcion, imagen) VALUES (null, @categoria_id, '', 00.00 , NULL, '');

CREATE TABLE `estados` (
 `id` int(3) NOT NULL AUTO_INCREMENT,
 `nombre` varchar(128) NOT NULL,
 PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

insert into estados values(null, 'Aguascalientes');
insert into estados values(null, 'Baja California');
insert into estados values(null, 'Baja California Sur');
insert into estados values(null, 'Campeche');
insert into estados values(null, 'Chiapas');
insert into estados values(null, 'Chihuahua');
insert into estados values(null, 'Coahuila');
insert into estados values(null, 'Colima');
insert into estados values(null, 'Distrito Federal');
insert into estados values(null, 'Durango');
insert into estados values(null, 'Estado de México');
insert into estados values(null, 'Guanajuato');
insert into estados values(null, 'Guerrero');
insert into estados values(null, 'Hidalgo');
insert into estados values(null, 'Jalisco');
insert into estados values(null, 'Michoacán');
insert into estados values(null, 'Morelos');
insert into estados values(null, 'Nayarit');
insert into estados values(null, 'Nuevo León');
insert into estados values(null, 'Oaxaca');
insert into estados values(null, 'Puebla');
insert into estados values(null, 'Querétaro');
insert into estados values(null, 'Quintana Roo');
insert into estados values(null, 'San Luis Potosí');
insert into estados values(null, 'Sinaloa');
insert into estados values(null, 'Sonora');
insert into estados values(null, 'Tabasco');
insert into estados values(null, 'Tamaulipas');
insert into estados values(null, 'Tlaxcala');
insert into estados values(null, 'Veracruz');
insert into estados values(null, 'Yucatán');
insert into estados values(null, 'Zacatecas');

--rollback
--DROP TABLE `estados`;

CREATE TABLE `clientes` (
 `id` int(11) NOT NULL AUTO_INCREMENT,
 `nombre` varchar(255) NOT NULL,
 `correo` varchar(255) NOT NULL,
 `password` varchar(128) NOT NULL,
 `passwordhash` varchar(512) NOT NULL,
 `razon` varchar(255) DEFAULT NULL,
 `calle` varchar(255) DEFAULT NULL,
 `numero` varchar(50) DEFAULT NULL,
 `colonia` varchar(128) DEFAULT NULL,
 `ciudad` varchar(128) DEFAULT NULL,
 `estado_id` int(3) DEFAULT NULL,
 `cp` int(5) DEFAULT NULL,
 `telefono` varchar(15) DEFAULT NULL,
 `celular` varchar(15) DEFAULT NULL,
 `rfc` varchar(13) DEFAULT NULL,
 `fecha_registro` TIMESTAMP NOT NULL DEFAULT now(),
 PRIMARY KEY (`id`),
 KEY `fk_clientes_estados0` (`estado_id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

--rollback
--DROP TABLE `clientes`;

CREATE TABLE `pedidos` (
 `id` int(11) NOT NULL AUTO_INCREMENT,
 `subtotal` decimal(11,0) NOT NULL,
 `costo_envio` decimal(11,0) NOT NULL,
 `cliente_id` int(11) NOT NULL,
 `session_id` varchar(256) NOT NULL,
 `fecha_registro` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
 PRIMARY KEY (`id`),
 KEY `fk_pedidos_clientes0` (`cliente_id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

--rollback
--DROP TABLE `pedidos`;

CREATE TABLE `pedidos_detalle` (
 `id` int(11) NOT NULL AUTO_INCREMENT,
 `pedido_id` int(11) NOT NULL,
 `producto_id` int(11) NOT NULL,
 `cantidad` int(11) NOT NULL,
 `precio` decimal(11,0) NOT NULL,
 PRIMARY KEY (`id`),
 KEY `fk_pedidos_detalle_pedidos0` (`pedido_id`),
 KEY `fk_pedidos_detalle_productos1` (`producto_id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

--rollback
--DROP TABLE `pedidos_detalle`;
