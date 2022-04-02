open Piece 
open Board

let puzzle1 = 
  [piece_of_tuple (General, Chu, (1,4));
   piece_of_tuple (Soldier, Han, (1,6));
   piece_of_tuple (General, Han, (9,5));
  ]

let puzzle2 = 
  [piece_of_tuple (General, Chu, (1,4));
   piece_of_tuple (Soldier, Han, (1,2));
   piece_of_tuple (General, Han, (9,5));
  ]

let puzzle3 = 
  [piece_of_tuple (Guard, Chu, (0,3));
   piece_of_tuple (Guard, Chu, (1,4));
   piece_of_tuple (Soldier, Han, (2,2));
   piece_of_tuple (Soldier, Han, (3,3));
   piece_of_tuple (General, Chu, (1,5));
   piece_of_tuple (General, Han, (9,4));
  ]

let puzzle4 = 
  [piece_of_tuple (General, Chu, (1,3));
   piece_of_tuple (Elephant, Chu, (2,4));
   piece_of_tuple (Soldier, Han, (3,3));
   piece_of_tuple (Soldier, Han, (3,5));
   piece_of_tuple (Soldier, Chu, (5,4));
   piece_of_tuple (Elephant, Han, (9,2));
   piece_of_tuple (General, Han, (9,4));
  ]

let puzzle5 = 
  [piece_of_tuple (General, Chu, (1,3));
   piece_of_tuple (Guard, Chu, (1,4));
   piece_of_tuple (Guard, Chu, (0,5));
   piece_of_tuple (Soldier, Han, (1,5));
   piece_of_tuple (Elephant, Chu, (2,4));
   piece_of_tuple (Soldier, Han, (2,1));
   piece_of_tuple (Soldier, Han, (2,2));
   piece_of_tuple (General, Han, (9,4));
  ]

let puzzle6 = 
  [piece_of_tuple (General, Chu, (1,5));
   piece_of_tuple (Elephant, Chu, (2,4));
   piece_of_tuple (Soldier, Han, (1,3));
   piece_of_tuple (Soldier, Han, (3,4));
   piece_of_tuple (Soldier, Han, (3,6));
   piece_of_tuple (Elephant, Chu, (4,2));
   piece_of_tuple (Horse, Chu, (4,3));
   piece_of_tuple (General, Han, (9,4));
  ]

let puzzle7 = 
  [piece_of_tuple (General, Chu, (0,3));
   piece_of_tuple (Guard, Chu, (1,4));
   piece_of_tuple (Horse, Han, (3,6));
   piece_of_tuple (General, Han, (9,4));
  ]

let puzzle8 = 
  [piece_of_tuple (General, Chu, (0,4));
   piece_of_tuple (Guard, Chu, (1,4));
   piece_of_tuple (General, Han, (7,3));
   piece_of_tuple (Guard, Han, (7,5));
   piece_of_tuple (Cannon, Han, (9,6));
  ]

let puzzle9 = 
  [piece_of_tuple (General, Chu, (0,4));
   piece_of_tuple (Chariot, Chu, (7,4));
   piece_of_tuple (Horse, Han, (2,5));
   piece_of_tuple (Cannon, Han, (7,3));
   piece_of_tuple (General, Han, (8,3));
  ]