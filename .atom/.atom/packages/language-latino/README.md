#Lenguaje latino soportado en Atom

Agrega resaltado de sintaxis y fragmentos de codigo latino en Atom.

###Para instalar siga las siguientes instrucciones:

![latino atom instalar](https://raw.githubusercontent.com/primitivorm/language-latino/master/latino_atom_instalar.png "latino atom instalar")

1. Abra su editor de texto Atom
2. Clic en el menu Archivo->Preferencias (File->Settings)
    o Editar->Preferencias (Edit->Settings) dependiendo su sistema operativo
3. Clic en Instalar (Install)
4. Abra un archivo de Latino extension (*.lat)

![latino atom](https://raw.githubusercontent.com/primitivorm/language-latino/master/latino_atom.png "latino atom")


Fragmentos de codigo (snippets)
---
| Desencadenador | Nombre                                  | codigo                                          |
|----------------|-----------------------------------------|-------------------------------------------------|
| si             | si condicional                          | si condicion #codigo fin                        |
| sn             | si sino condicional                     | si condicion #codigo sino #codigo fin           |
| mientras       | ciclo mientras                          | mientras condition #codigo fin                  |
| hacer          | ciclo hacer                             | hacer #codigo mientras condicion                |
| desde          | ciclo desde                             | desde ( i = 0; i < 10; i++) #codigo fin         |
| fun            | funcion                                 | funcion nombre_funcion (argumentos) #codigo fin |
| ret            | retorno                                 | retorno valor                                   |
| esc            | escribir                                | escribir("mensaje")                             |
| inc            | incluir                                 | incluir "modulo"                                |

Licencia
---
Latino Atom Plugin se distribuye bajo la licencia MIT.

####Cualquier aportacion o sugerencia es bienvenida.
