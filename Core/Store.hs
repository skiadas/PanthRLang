-- module implementing functions related to store fetches

module Store (Store, emptyStore) where
    
type Loc = Integer
type Store a = [(Loc, a)]

emptyStore = [] :: Store a