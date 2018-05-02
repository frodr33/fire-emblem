open Types


let map1 =
  {
    width = 15;
    length = 10;
    grid =
      [|

    (* Top row *)
    [|
      {coordinate = (26 * 0, 26 * 0); ground = Wall; tile_type = Wall1};
      {coordinate = (26 * 1, 26 * 0); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 2, 26 * 0); ground = Wall; tile_type = Wall2};
      {coordinate = (26 * 3, 26 * 0); ground = Wall; tile_type = Wall3};
      {coordinate = (26 * 4, 26 * 0); ground = Wall; tile_type = Wall4};
      {coordinate = (26 * 5, 26 * 0); ground = Forest; tile_type = Tree};
      {coordinate = (26 * 6, 26 * 0); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 7, 26 * 0); ground = Peaks; tile_type = Bush};
      {coordinate = (26 * 8, 26 * 0); ground = Peaks; tile_type = Bush};
      {coordinate = (26 * 9, 26 * 0); ground = Peaks; tile_type = Bush};
      {coordinate = (26 * 10, 26 * 0); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 11, 26 * 0); ground = Ocean; tile_type = Water1};
      {coordinate = (26 * 12, 26 * 0); ground = Mountain; tile_type = Darkbush};
      {coordinate = (26 * 13, 26 * 0); ground = Mountain; tile_type = Darkbush};
      {coordinate = (26 * 14, 26 * 0); ground = Mountain; tile_type = Darkbush};
    |];

(* 2nd row *)
    [|
      {coordinate = (26 * 0, 26 * 1); ground = Wall; tile_type = Wall6};
      {coordinate = (26 * 1, 26 * 1); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 2, 26 * 1); ground = Wall; tile_type = Wall6};
      {coordinate = (26 * 3, 26 * 1); ground = Wall; tile_type = Wall6};
      {coordinate = (26 * 4, 26 * 1); ground = Wall; tile_type = Wall5};
      {coordinate = (26 * 5, 26 * 1); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 6, 26 * 1); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 7, 26 * 1); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 8, 26 * 1); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 9, 26 * 1); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 10, 26 * 1); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 11, 26 * 1); ground = Ocean; tile_type = Water2};
      {coordinate = (26 * 12, 26 * 1); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 13, 26 * 1); ground = Plain; tile_type = Grass};
      {coordinate = (26 * 14, 26 * 1); ground = Mountain; tile_type = Darkbush};
  |];


(* 3rd row *)
  [|
    {coordinate = (26 * 0, 26 * 2); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 1, 26 * 2); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 2, 26 * 2); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 3, 26 * 2); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 4, 26 * 2); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 5, 26 * 2); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 6, 26 * 2); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 7, 26 * 2); ground = Wall; tile_type = Crack};
    {coordinate = (26 * 8, 26 * 2); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 9, 26 * 2); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 10, 26 * 2); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 11, 26 * 2); ground = Plain; tile_type = Bridge};
    {coordinate = (26 * 12, 26 * 2); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 13, 26 * 2); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 14, 26 * 2); ground = Plain; tile_type = Grass};
  |];

(* 4th row *)
  [|
    {coordinate = (26 * 0, 26 * 3); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 1, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 2, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 3, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 4, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 5, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 6, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 7, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 8, 26 * 3); ground = Wall; tile_type = Crack};
    {coordinate = (26 * 9, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 10, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 11, 26 * 3); ground = Ocean; tile_type = Water2};
    {coordinate = (26 * 12, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 13, 26 * 3); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 14, 26 * 3); ground = Plain; tile_type = Grass};
  |];

(* 5th row *)
  [|
    {coordinate = (26 * 0, 26 * 4); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 1, 26 * 4); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 2, 26 * 4); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 3, 26 * 4); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 4, 26 * 4); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 5, 26 * 4); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 6, 26 * 4); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 7, 26 * 4); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 8, 26 * 4); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 9, 26 * 4); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 10, 26 * 4); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 11, 26 * 4); ground = Ocean; tile_type = Water3};
    {coordinate = (26 * 12, 26 * 4); ground = Ocean; tile_type = Water4};
    {coordinate = (26 * 13, 26 * 4); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 14, 26 * 4); ground = Plain; tile_type = Grass};
  |];

(* 6th row *)
  [|
    {coordinate = (26 * 0, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 1, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 2, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 3, 26 * 5); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 4, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 5, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 6, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 7, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 8, 26 * 5); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 9, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 10, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 11, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 12, 26 * 5); ground = Plain; tile_type = Bridge};
    {coordinate = (26 * 13, 26 * 5); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 14, 26 * 5); ground = Plain; tile_type = Grass};
  |];

(* 7th row *)
  [|
    {coordinate = (26 * 0, 26 * 6); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 1, 26 * 6); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 2, 26 * 6); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 3, 26 * 6); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 4, 26 * 6); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 5, 26 * 6); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 6, 26 * 6); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 7, 26 * 6); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 8, 26 * 6); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 9, 26 * 6); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 10, 26 * 6); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 11, 26 * 6); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 12, 26 * 6); ground = Ocean; tile_type = Water3};
    {coordinate = (26 * 13, 26 * 6); ground = Ocean; tile_type = Water4};
    {coordinate = (26 * 14, 26 * 6); ground = Plain; tile_type = Grass};
  |];

(* 8th row *)
  [|
    {coordinate = (26 * 0, 26 * 7); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 1, 26 * 7); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 2, 26 * 7); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 3, 26 * 7); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 4, 26 * 7); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 5, 26 * 7); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 6, 26 * 7); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 7, 26 * 7); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 8, 26 * 7); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 9, 26 * 7); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 10, 26 * 7); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 11, 26 * 7); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 12, 26 * 7); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 13, 26 * 7); ground = Ocean; tile_type = Water1};
    {coordinate = (26 * 14, 26 * 7); ground = Plain; tile_type = Grass};
  |];

(* 9th row *)
  [|
    {coordinate = (26 * 0, 26 * 8); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 1, 26 * 8); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 2, 26 * 8); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 3, 26 * 8); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 4, 26 * 8); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 5, 26 * 8); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 6, 26 * 8); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 7, 26 * 8); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 8, 26 * 8); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 9, 26 * 8); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 10, 26 * 8); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 11, 26 * 8); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 12, 26 * 8); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 13, 26 * 8); ground = Ocean; tile_type = Water5};
    {coordinate = (26 * 14, 26 * 8); ground = Ocean; tile_type = Water6};
  |];

(* 10th row *)
  [|
    {coordinate = (26 * 0, 26 * 9); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 1, 26 * 9); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 2, 26 * 9); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 3, 26 * 9); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 4, 26 * 9); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 5, 26 * 9); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 6, 26 * 9); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 7, 26 * 9); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 8, 26 * 9); ground = Forest; tile_type = Tree};
    {coordinate = (26 * 9, 26 * 9); ground = Plain; tile_type = Grass};
    {coordinate = (26 * 10, 26 * 9); ground = Ocean; tile_type = Water8};
    {coordinate = (26 * 11, 26 * 9); ground = Ocean; tile_type = Water9};
    {coordinate = (26 * 12, 26 * 9); ground = Ocean; tile_type = Water8};
    {coordinate = (26 * 13, 26 * 9); ground = Ocean; tile_type = Water7};
    {coordinate = (26 * 14, 26 * 9); ground = Ocean; tile_type = Water7};
  |]

  |]
  }

let test =
  {
    width = 3;
    length = 3;
    grid =
      [|
        [|
          {coordinate = (26 * 0, 26 * 0); ground = Plain; tile_type = Grass};
          {coordinate = (26 * 1, 26 * 0); ground = Plain; tile_type = Grass};
          {coordinate = (26 * 2, 26 * 0); ground = Plain; tile_type = Grass};
        |];
        [|
          {coordinate = (26 * 0, 26 * 1); ground = Plain; tile_type = Grass};
          {coordinate = (26 * 1, 26 * 1); ground = Plain; tile_type = Grass};
          {coordinate = (26 * 2, 26 * 1); ground = Plain; tile_type = Grass};
        |];
        [|
          {coordinate = (26 * 0, 26 * 2); ground = Plain; tile_type = Grass};
          {coordinate = (26 * 1, 26 * 2); ground = Plain; tile_type = Grass};
          {coordinate = (26 * 2, 26 * 2); ground = Plain; tile_type = Grass};
        |]

      |]
  }