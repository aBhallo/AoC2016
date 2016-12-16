import qualified AStar

main = do  
        let input = 1350
            result = compute input
        print result

type Maze = [[Bool]]       -- True if walkable
type Coord = (Int, Int)    -- (x co-ord, y co-ord)
type State = (Coord, Maze) 

initialState :: State
initialState = ((1,1),[[]])   

compute input = initialState


