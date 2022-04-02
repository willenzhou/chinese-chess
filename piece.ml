type genre = Soldier | Cannon | Horse | Elephant | Guard | General | Chariot
type team = Han | Chu

type t = {
  genre : genre;
  team : team;
  position: (int * int)
}

let get_genre t = 
  t.genre

let get_team t = 
  t.team

let get_pos t = 
  t.position

let piece_of_tuple info = 
  match info with
  | (genre, team, pos) -> {
      genre = genre;
      team = team;
      position = pos
    }