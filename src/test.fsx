open roguelike

let mur = Wall(1,2)

let vand = Water (4,5)
let ild = Fire(10,15)

let ex = Exit(10,10)
let W = World(50, 25)

let z = Zombie(20,10)

W.AddItem mur
W.AddItem vand
W.AddItem ild
W.AddItem ex
W.AddItem z
W.Play()