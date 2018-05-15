# Fire-Emblem

![screenshot](Sprites/screenshot.PNG)

## Description
Fire-Emblem is a clone of the real game boy game Fire Emblem. Fire-Emblem is a turn based game where the user controlls several blue-colored 'Players'. The goal of the game is to kill the enemies without dying. 

## How to install
OCaml and Opam must be installed prior to installing this project. If OCaml and Opam are not installed then follow the 
tutorial at `http://www.cs.cornell.edu/courses/cs3110/2018sp/install.html`

If Ocaml and Opam are installed, then do the following steps:
	1. Clone the repository
	2. Enter `opam install js_of_ocaml js_of_ocaml-ocamlbuild js_of_ocaml-camlp4 js_of_ocaml-lwt` in terminal/command line
	3. In the terminal/command line enter `make`
	4. Click on `index.html` inside the repository directory
	5. A window in your default browser should open and fire-emblem should play automatically! (Click on the screen if nothing is happening)

## How to Play
### Selection
When in the game, you will see various players, enemies, items, and menus. Using 'Z' you can select players to move by pressing on the currently active character. You can tell who the currently active character is by pressing 'A'. 'A' will automatically transfer the cursor over to the currently active character. You can also use 'Z' on enemies to see their range of movement, on menus when you need to select a choice on the menu, and on the ground when you want to end your turn. You can press 'X' to deselect something that has already been selected.

### Movement
When you press 'Z' when the cursor is on a player, an arrangement of blue and red tiles will appear. The blue tiles signify where the current character can move to. Red tiles signify tiles that the player can attack but cannot move to.

### Attacking
After movement, you have the option to attack. If you click on Attack, you will then have to choose an item to attack with. Once you choose an item, red tiles will appear on the map signifying where attacking is possible. If an enemy is on the red tile, simply press 'Z' when the cursor is on that tile. If no enemies are on any red tiles, you must deselect using 'X' and choose another option

## External Dependencies
Oscigen's js_of_ocaml library was used to build the GUI. The library translates Ocaml code to javascript and allows for more advanced GUI abilities than standard Ocaml GUI libraries

## Authors
- Frank Rodriguez [@frodr33](https://github.com/frodr33)
- Albert Tsao
- Darren Tsai 
- Ray Gu