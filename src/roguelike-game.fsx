open roguelike

let W = World(50, 25)

W.OpenLevel("hot_and_cold.txt")
W.Play()