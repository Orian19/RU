import java.util.ArrayList;
import java.util.List;

/**
 * SplayTree.java
 * Implementation of a keyless Splay tree for representing a seuqence
 * of characters
 */
public class SplayTree {

    /**
     * Represents a node in the SplayTree.
     */
    public class Node {
        int size;
        Node[] children;
        Node parent;
        char character;
        int swap;

        /**
         * Constructs a new Node with the given character.
         * @param character the character to store in this Node.
         */
        public Node(char character) {
            this.parent = null;
            this.character = character; // "value"
            this.size = 1;              // size of the subtree rooted at the node (augmented data)
            this.children = new Node[2];
            this.swap = 0; // initializing swap field (can be 0/1)
        }

        //TODO: remove the following functions before submitting

        public Node getLeft() {
            Node temp = parent;
            int idx = swap;
            while (temp != null) {
                idx = idx ^ temp.swap;
                temp = temp.parent;
            }
            return children[idx] != null ? children[idx] : null;
        }

        public Node getRight() {
            Node temp = parent;
            int idx = swap;
            while (temp != null) {
                idx = idx ^ temp.swap;
                temp = temp.parent;
            }
            return children[idx ^ 1] != null ? children[idx ^ 1] : null;
        }
    }

    // The root of the SplayTree
    Node root;

    /**
     * Constructs an empty SplayTree.
     */
    public SplayTree() {
        this.root = null;
    }
    
    
    /**
     * Constructs a SplayTree from the given string.
     * @param s the string to use for constructing the tree.
     */
    public SplayTree(String s) {
        int len = s.length();

        if (len == 0) { // checking if the string is empty
            this.root = null;
            return;
        }
        this.root = buildBST(s, 0, len-1); // building the balanced BST
    }

    /**
     * building a BST from a sorted array using a recursion
     * the key idea is to take the median to be the root recursively
     * (if an inorder traversal is done the result will be the original permutation of the string)
     *
     * @param s the string from which we create the BST (each node is a char from s)
     * @param start index
     * @param end index
     * @return the root node of the BST
     */
    private Node buildBST(String s, int start, int end) {
        if (start > end) return null;

        int median = (start + end) / 2;
        Node newNode = new Node(s.charAt(median)); // taking the median to be the root

        newNode.children[0] = buildBST(s, start, median-1); // recursively handling left children
        // updating the parent node and size for a left child
        if (newNode.children[0] != null) {
            newNode.children[0].parent = newNode;
            newNode.size += newNode.children[0].size;
        }

        newNode.children[1] = buildBST(s, median+1, end); // recursively handling right children
        // updating the parent node and size for a right child
        if (newNode.children[1] != null) {
            newNode.children[1].parent = newNode;
            newNode.size += newNode.children[1].size;
        }
        return newNode; // returning the newNode which represents the root of the tree
    }

    /**
     * Join Constructor.
     * Creates a new splay tree by joining two existing splay trees T1 and T2.
     * The inorder of nodes of the new tree is the concatenation of the inorders of T1 and of T2.
     * This operation invalidates T1 and T2.
     *
     * @param T1 The splay tree containing nodes less than those in T2.
     * @param T2 The splay tree containing nodes greater than those in T1.
     */
    public SplayTree(SplayTree T1, SplayTree T2) {
        // If T1 is null or its root is null, use T2 as the new splay tree
        if (null == T1 || null == T1.root){
            this.root = T2.root;
            return;
        }

        // If T2 is null or its root is null, use T1 as the new splay tree
        if (null == T2 || null == T2.root) {
            this.root = T1.root;
            return;
        }
        
        // Splay the rightmost("largest") node of T1 to the root so it has no left child
        Node max = T1.root;
        int sAccum = T1.root.swap;
        while(max.children[sAccum^ 1] != null) {
            max = max.children[sAccum ^ 1];
            sAccum = (max.swap + sAccum) % 2;
        }
        T1.splay(max);

        // Make T2 the right subtree of the root of T1
        T2.root.swap = T2.root.swap ^ T1.root.swap;

        T1.root.children[T1.root.swap^ 1] = T2.root;
        T2.root.parent = T1.root;
        updateSize(T1.root);
        this.root = T1.root;

        // Invalidate T1 and T2
        T1.root = T2.root = null;
    }
    
    /**
     * Update the size field of the given Node based on its children
     * @param x the Node to update
     */
    private void updateSize(Node x) {
        x.size = 1 + size(x.children[0]) + size(x.children[1]);
    }

    /**
     * Get the size of the given Node (protected for null arguments)
     * @param x the Node to measure
     * @return the size of the Node
     */
    private int size(Node x) {
        return (x == null) ? 0 : x.size;
    }

    
    /**
     * Perform a rotation operation between a Node x in the Splay Tree
     * and its parent.
     * @param x the Node to rotate
     */
    private void rotate(Node x) {
        Node p = x.parent;
        Node gp = p.parent;

        //idx is index of x in p's children
        int idx = p.children[1] == x ? 1 : 0;

        //set x as child of gp instead of p
        if (gp != null) {
            gp.children[gp.children[1] == p ? 1 : 0] = x;
        }
        x.parent = gp;

        // x's child in the direction opposite to idx becomes the child of p instead of x
        p.children[idx] = x.children[idx ^ 1 ^ x.swap];
        if (p.children[idx] != null) {
            p.children[idx].parent = p;
            // TODO: explain
            // updating swap
            p.children[idx].swap = (p.children[idx].swap + x.swap) % 2;
        }

        //p becomes the child of x in the position opposite to idx
        x.children[idx ^ 1 ^ x.swap] = p;
        p.parent = x;

        // updating swap values of x and p //TODO: explain in details
        x.swap = (x.swap + p.swap) % 2;
        p.swap = (p.swap + x.swap) % 2;

        //update the sizes of p then x
        updateSize(p);
	    updateSize(x);

        //update the root if necessary
        if (root == p) {
            root = x;
        }
    }

    

    /**
     * Performs a splay operation on the given node, bringing it to the root of the tree 
     * using zigzig and zigzag operations.
     * @param x The node to be splayed.
     */
    private void splay(Node x) { // TODO: probably need to update swap here (zig-zig, zig-zag). maybe already handled in rotate?
        //Keep rotating until x is the root.
        int sAccum_p = 0;
        int sAccum_gp = 0;
        while (x.parent != null) {

            Node p = x.parent; //parent
            Node gp = p.parent; //grandparent

            sAccum_p = (p.swap + sAccum_p) % 2;

            // If the node has a grandparent, determine the type of rotation needed.
            if (gp != null) {
                sAccum_p = (gp.swap + sAccum_gp) % 2;

                // If the relation between gp and p is the same as the relation between p and x - zigzig
                boolean zigzig = (gp.children[sAccum_gp ^ 1] == p) == (p.children[sAccum_p ^ 1] == x);
                if (zigzig) {
		            rotate(p); // In zigzig, rotate p first.
                } else {
                    rotate(x); // In zigzag, rotate x first.
                }
            }

            //in zigzig, zigzag, and when x is a child of the root, the last rotation is of x
            rotate(x);
        }
    }
    
    /**
     * Splays and returns the node whose rank is k in the tree.
     * Rank is defined by the position of a node in the in-order traversal of the tree.
     *
     * @param k The rank of the node to be selected.
     * @return The node at rank k, or null if such a node does not exist.
     */
    public Node select(int k) {
        Node x = root;

        // swap accumulator
        int sAccum = 0;
        while (x != null) {
            sAccum = (x.swap + sAccum) % 2;
            int t = size(x.children[sAccum]); // Size of left subtree //TODO: update (swap)
            if (t > k) {
                // If left subtree has more than k nodes, go to the left subtree
                x = x.children[sAccum]; //TODO: update (swap)
            } else if (t < k) {
                // If left subtree has fewer than k nodes, go to the right subtree
                x = x.children[sAccum ^ 1]; //TODO: update (swap)
                k = k - t - 1; //relative rank in the right subtree
            } else {
                // similar to find operation
                // If the size of the subtree is equal to k, x is the desired node
                // Splay x and return it
                splay(x);
                return x;
            }
        }
        // The node with rank k doesn't exist (k is out of bounds).
        return null;
    }

    /**
     * Splits the current splay tree into two splay trees T1 and T2 around a node x,
     * where T1 contains all nodes whose rank is smaller than x, and T2 contains nodes 
     * whose rank is at least the rank of x.
     * This operation invalidates the current tree.
     *
     * @param x The node around which the split operation is performed.
     * @return An array containing the T1 and T2 in this order.
     */
    public SplayTree[] split(Node x) {
	
        // After splaying x, T1 is the left subtree and T2 is x and its right subtree.
        splay(x);

        SplayTree T1 = new SplayTree();
        T1.root = x.children[x.swap]; //TODO: explain
        if (T1.root != null) {
            T1.root.parent = null;
            T1.root.swap ^= x.swap; //TODO: explain
        }

        // Create a new splay tree for the right subtree of x (nodes greater than or equal to x)
        SplayTree T2 = new SplayTree();
        T2.root = x; // x is included in the right subtree after splitting according to x
        T2.root.children[x.swap] = null; //TODO: explain
        updateSize(T2.root);

        //invalidate current tree
        this.root = null;
        
        return new SplayTree[] {T1, T2};
    }


    /**
     * Set the i’th character in the sequence to be character c.
     * 
     * @param i The index of the character to be replaced in the splay tree. Index starts from 1.
     * @param c The new character to replace the old character.
     */
    public void substitute(int i, char c) {
        // Check if the index i is within the valid range.
        if (i < 0 || i >= this.root.size) {
	    System.out.println("Invalid index: " + i);
            return;
        }

        // Select the node at index i.
        Node node = select(i);

        // Replace the character in the selected node with c.
        node.character = c; 
    }

    /**
     * Insert the character c at position i in the sequence (shifting all the following
    characters).
     * This method creates a new splay tree t_c containing only the node to be inserted.
     * Then, it joins the current tree with t_c, which places the new character at the end of the tree.
     * If the target position is not at the end, the method translocates the new character to the target position.
     *
     * @param i The position at which the character is to be inserted.
     * @param c The character to be inserted.
     */
    public void insert(int i, char c){

        // Check if the index i is within the valid range.
        if (i < 0 || i > this.root.size) {
            throw new IllegalArgumentException("Invalid index: " + i);
        }

        // Create a new splay tree containing only the character to be inserted.
        SplayTree t_c = new SplayTree(String.valueOf(c));

        // Join the current tree with t_c.
        SplayTree new_t = new SplayTree(this, t_c);

        // Get the max rank of the new tree.
        int max_rank = new_t.root.size-1;

        // Update the root of the current tree.
        this.root = new_t.root;

        // If the target position is not at the end, translocate the new character to the target position.
        if (i < max_rank)
            translocate(max_rank,max_rank, i);	
    }


    /**
     * Remove the i’th character from the sequence (shifting all the following characters)
     * @param i The index of the node to be deleted from the splay tree. Index starts from 0.
     */
    public void delete(int i) { //todo: need to test
        // Check if the index i is within the valid range.
        if (i < 0 || i >= this.root.size) {
            throw new IllegalArgumentException("Invalid index: " + i);
        }

        // Splay the node x with rank i.
        Node x = select(i);
        
        // Disconnect x from its children
        SplayTree t1 = new SplayTree();
        SplayTree t2 = new SplayTree();
        t1.root = x.children[x.swap]; //TODO: update (swap)
        if (t1.root != null) {
            t1.root.parent = null;
        }   
        t2.root = x.children[x.swap ^ 1];
        if (t2.root != null) {
            t2.root.parent = null;
        }
        x.children[x.swap] = x.children[x.swap ^ 1] = null;  //TODO: update (swap)

        // Join the two subtrees
        SplayTree t = new SplayTree(t1,t2);
        this.root = t.root;
    }

    /**
     * Move the subsequence starting at i and ending at j to start at position k. 
     * k is relative to the sequence prior to the operation, and is guaranteed not to be in the range [i,j].
     * This method selects three nodes x_i, x_j, and x_k corresponding to the indices i, j, and k, respectively.
     * Then, it splits the tree at x_i and x_j, effectively separating the sequence from i to j.
     * The tree is then restructured by joining the left part (up to i), the right part (after j), and the selected subsequence at the new position k.
     *
     * @param i The start index (rank) of the subsequence to be translocated.
     * @param j The end index (rank) of the subsequence to be translocated.
     * @param k The index (rank) to which the subsequence is to be translocated.
     */

     public void translocate(int i, int j, int k){

        // Deal with invalid arguments
        if (i < 0 || j < i || j >= this.root.size || k < 0 || k > this.root.size) {
            throw new IllegalArgumentException("Invalid index: " + k);
        }
        if (k >= i && k <= j) {
            throw new IllegalArgumentException("Invalid index: " + k);
        }
        
        Node x_i, x_j, x_k;
        x_i = select(i);
        x_j = select(j+1); //select j+1 because we want to split immediately after j (not at j)
        x_k = select(k);

        // Split the tree at x_i.
        // tt[0] contains nodes with ranks smaller than i  [...i)
        // tt[1] contains nodes with ranks at least i  [i...]
        SplayTree[] tt = split(x_i);  //tt[0] is [0...i), tt[1] is [i...]

        SplayTree t,t_ij;
        SplayTree[] tt2;

        if (x_j != null) {
            // j is not the maximum rank
            // Split the [i...] part at x_j.
            // tt2[0] contains nodes with ranks from i to j (including)   [i...j]
            // tt2[1] contains nodes with ranks greater than j  (j...]
            tt2 = tt[1].split(x_j); //tt2[0] is [i,j], tt2[1] is (j...]	

            // Join [0...i) with (j...]
            t = new SplayTree(tt[0],tt2[1]); //t contains all nodes not in [i...j]
            t_ij = tt2[0];  //t_ij contains [i...j]
        }
        else {
            // j is the maximum element, so there is no (j...] part
            t = tt[0]; //t contains [0...i)
            t_ij = tt[1]; //t_ij contains [i...j] = [i...]
        }

        SplayTree[] tt3;

        if (x_k != null) {
            //k is not one location past the length of the sequence

            // Split the joined tree t at node x_k.
            tt3 = t.split(x_k);
            
            // Join the left part of the tree (up to k), the selected subsequence, and the remaining part of the tree.
            t = new SplayTree(tt3[0],t_ij);
            t = new SplayTree(t,tt3[1]);
        }
        else {
            //Otherwise should translocate [i,j] to the very end of the sequence
            t = new SplayTree(t,t_ij);
        }
        
        this.root = t.root;
	
    }

    /**
     * Reverse the subsequences [i...j] of the splay tree.
     * @param i The starting index of the range to be inverted.
     * @param j The ending index of the range to be inverted.
     */
    public void invert(int i, int j){
        // Deal with invalid arguments
        if (i < 0 || j < i || j >= this.root.size) {
            throw new IllegalArgumentException("Invalid indices: " + i + j);
        }

        Node x_i, x_j;
        x_i = select(i);
        x_j = select(j+1);

        // Split the tree at x_i.
        // tt[0] contains nodes with ranks smaller than i  [...i)
        // tt[1] contains nodes with ranks at least i  [i...]
        SplayTree[] tt = split(x_i);  // tt[0] is [0...i), tt[1] is [i...]

        SplayTree t_ij;
        SplayTree t_i = tt[0];
        SplayTree[] tt2 = new SplayTree[0];

        if (x_j != null) {
            // j is not the maximum rank
            // Split the [i...] part at x_j.
            // tt2[0] contains nodes with ranks from i to j (including)   [i...j]
            // tt2[1] contains nodes with ranks greater than j  (j...]
            tt2 = tt[1].split(x_j); // tt2[0] is [i,j], tt2[1] is (j...]

            // Join [0...i) with (j...]
            t_ij = tt2[0];  //t_ij contains [i...j]
        }
        else {
            // j is the maximum element, so there is no (j...] part
            t_ij = tt[1]; //t_ij contains [i...j] = [i...]
        }

        //inverting t_ij as needed
        t_ij.root.swap = 1 - t_ij.root.swap;

        //joining all the trees back together
        SplayTree st = new SplayTree(t_i, t_ij); // joining "T1","T2"
        if (x_j != null) {
            st = new SplayTree(st, tt2[1]); // "joining st with "T3" (if exists)
        }

        // updating the root to be the root of the new inverted tree
        this.root = st.root;
    }
    
    /**
     * Returns the sequence of characters represented by the data structure. That is, the characters stored
     * at each node in inorder.
     *
     * @return The sequence of characters represented by the tree.
     */
    public String toString() {
        return toString(root);
    }


    /**
     * Helper method for toString(). 
     * Recursively collects the characters of the nodes of the tree rooted at x using inorder traversal.
     *
     * @param x The root of the subtree to be visited.
     * @return The sequence of characters represented by the subtree rooted at x.
     */
    public String toString(Node x) {
        return toString(x, 0);
    }

    public String toString(Node x, int sAccum) {
        if (x == null) {
            return "";
        }
        int curS = (x.swap + sAccum) % 2;
        return toString(x.children[curS], curS) + x.character + toString(x.children[curS ^ 1], curS); //TODO: update (swap)
    }

    /*
     * The main method used for testing.
     */
    public static void main(String[] args) {
        //test invert
        TreePrinter newT1 = new TreePrinter();
        TreePrinter newT2 = new TreePrinter();
        TreePrinter newT3 = new TreePrinter();
        TreePrinter newT4 = new TreePrinter();
        TreePrinter newT5 = new TreePrinter();

        String s1 = "1234", s2 = "1234567890", s3 = "12345", s4 = "1234567", s5 = "abcdefghij";

        SplayTree st1 = new SplayTree(s1);
        newT1.print(st1.root);
        SplayTree st2 = new SplayTree(s2);
        newT2.print(st2.root);
        SplayTree st3 = new SplayTree(s3);
        newT3.print(st3.root);
        SplayTree st4 = new SplayTree(s4);
        newT4.print(st4.root);
        SplayTree st5 = new SplayTree(s5);
        newT5.print(st5.root);

        st1.invert(0, s1.length()-1);
        st2.invert(0, s2.length()-1);
        st3.invert(0, s3.length()-1);
        st4.invert(0, s4.length()-1);

        st5.invert(1, 7);
//        st5.invert(0, 7-1);

        System.out.println("inverting: " + s1 + "     results:" + st1.toString(st1.root));
        System.out.println("inverting: " + s2 + "     results:" + st2.toString(st2.root));
        System.out.println("inverting: " + s3 + "     results:" + st3.toString(st3.root));
        System.out.println("inverting: " + s4 + "     results:" + st4.toString(st4.root));
        System.out.println("inverting: " + s5 + "     results:" + st5.toString(st5.root) +
                "    should be: " + "gfedcbahij" + "   success?  " + (st5.toString(st5.root)).equals("gfedcbahij"));

        newT1.print(st1.root);
        newT2.print(st2.root);
        newT3.print(st3.root);
        newT4.print(st4.root);
        newT5.print(st5.root);
        //TODO: think about and check edge cases!
    }



    //TODO: delete when submitting
    /**
     * Binary tree printer
     *
     * @author MightyPork
     */
    public static class TreePrinter {

        /**
         * Print a tree
         *
         * @param root
         *             tree root node
         */
        public void print(Node root) {
            List<List<String>> lines = new ArrayList<>();

            List<Node> level = new ArrayList<>();
            List<Node> next = new ArrayList<>();

            level.add(root);
            int nn = 1;

            int widest = 0;

            while (nn != 0) {
                List<String> line = new ArrayList<>();

                nn = 0;

                for (Node n : level) {
                    if (n == null) {
                        line.add(null);

                        next.add(null);
                        next.add(null);
                    } else {
                        String aa = "" + n.character;
                        line.add(aa);
                        if (aa.length() > widest)
                            widest = aa.length();

                        next.add(n.getLeft());
                        next.add(n.getRight());

                        if (n.getLeft() != null)
                            nn++;
                        if (n.getRight() != null)
                            nn++;
                    }
                }

                if (widest % 2 == 1)
                    widest++;

                lines.add(line);

                List<Node> tmp = level;
                level = next;
                next = tmp;
                next.clear();
            }

            int perpiece = lines.get(lines.size() - 1).size() * (widest + 4);
            for (int i = 0; i < lines.size(); i++) {
                List<String> line = lines.get(i);
                int hpw = (int) Math.floor(perpiece / 2f) - 1;

                if (i > 0) {
                    for (int j = 0; j < line.size(); j++) {

                        // split node
                        char c = ' ';
                        if (j % 2 == 1) {
                            if (line.get(j - 1) != null) {
                                c = (line.get(j) != null) ? '┴' : '┘';
                            } else {
                                if (j < line.size() && line.get(j) != null)
                                    c = '└';
                            }
                        }
                        System.out.print(c);

                        // lines and spaces
                        if (line.get(j) == null) {
                            for (int k = 0; k < perpiece - 1; k++) {
                                System.out.print(" ");
                            }
                        } else {

                            for (int k = 0; k < hpw; k++) {
                                System.out.print(j % 2 == 0 ? " " : "─");
                            }
                            System.out.print(j % 2 == 0 ? "┌" : "┐");
                            for (int k = 0; k < hpw; k++) {
                                System.out.print(j % 2 == 0 ? "─" : " ");
                            }
                        }
                    }
                    System.out.println();
                }

                // print line of numbers
                for (int j = 0; j < line.size(); j++) {

                    String f = line.get(j);
                    if (f == null)
                        f = "";
                    int gap1 = (int) Math.ceil(perpiece / 2f - f.length() / 2f);
                    int gap2 = (int) Math.floor(perpiece / 2f - f.length() / 2f);

                    // a number
                    for (int k = 0; k < gap1; k++) {
                        System.out.print(" ");
                    }
                    System.out.print(f);
                    for (int k = 0; k < gap2; k++) {
                        System.out.print(" ");
                    }
                }
                System.out.println();

                perpiece /= 2;
            }
        }
    }

}

