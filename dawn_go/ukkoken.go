package main

import "fmt"

// On-line construction of suffix trees, Ukkoken '95

type node struct {
    start      int
    len        int
    children   map[rune]*node
    suffixLink *node
    parent     *node
}

func createNode(start, len int, parent *node) *node {
    return &node{
        start:      start,
        len:        len,
        children:   make(map[rune]*node),
        suffixLink: nil,
        parent:     parent,
    }
}

func splitNode(n *node, start, len int, nodeLabel rune) *node {
    newNode := createNode(start, len, n)
    remainingLen := n.len - len - 1
    n.len = len
    child := createNode(n.start+len+1, remainingLen, newNode)
    n.children[nodeLabel] = child
    newNode.suffixLink = n.suffixLink
    n.suffixLink = newNode
    return newNode
}

func findNode(root *node, start, len int, s string) *node {
    if len == 0 {
        return root
    }
    child, ok := root.children[rune(s[start])]
    if !ok {
        return nil
    }
    childLen := child.len
    if childLen >= len {
        return child
    }
    return findNode(child, child.start+childLen+1, len-childLen-1, s)
}

func followLinks(n *node) *node {
    if n.suffixLink == nil {
        return n
    }
    return followLinks(n.suffixLink)
}

func updateTree(s string, i int, root *node) {
    len := len(s)
    n := followLinks(root)
    remaining := len - i
    if remaining > 0 {
        leaf := createNode(i, remaining, n)
        nodeLabel := rune(s[i])
        child, ok := n.children[nodeLabel]
        if ok {
            start := child.start
            len := child.len
            foundNode := findNode(root, start, len, s)
            if foundNode != nil && s[start:start+len] == s[i:i+len] {
                updateTree(s, i+len+1, child)
            } else if foundNode != nil {
                newNode := splitNode(foundNode, start, len, nodeLabel)
                leaf = createNode(i, remaining, newNode)
                newNode.children[nodeLabel] = leaf
            } else {
                n.children[nodeLabel] = leaf
            }
        } else {
            n.children[nodeLabel] = leaf
        }
    }
}

func buildSuffixTree(s string) *node {
    root := createNode(0, 0, nil)
    for i := range s {
        updateTree(s, i, root)
    }
    return root
}