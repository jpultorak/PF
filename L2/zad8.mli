module type PRIOQUEUE = 
    sig 
		type 'a lheap
        val init_tree : 'a -> 'a lheap
    	val insert : 'a lheap -> 'a -> 'a lheap
   	 	val delete_min : 'a lheap -> 'a lheap
    	val get_min : 'a lheap -> 'a
	end;;
