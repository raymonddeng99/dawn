package main

import (
	"hash/fnv"
	"math/bits"
)

type BloomFilter struct {
	size      uint
	vector    []byte
	hashFuncs []uint64
}

func NewBloomFilter(size uint, hashFuncs []uint64) *BloomFilter {
	vector := make([]byte, (size+7)/8)
	return &BloomFilter{size, vector, hashFuncs}
}

func (bf *BloomFilter) Add(element string) {
	for _, hashFunc := range bf.hashFuncs {
		hash := fnv.New64a()
		hash.Write([]byte(element))
		bitIndex := uint(hash.Sum64()) % bf.size
		bf.vector[bitIndex/8] |= 1 << (bitIndex % 8)
	}
}

func (bf *BloomFilter) Contains(element string) bool {
	for _, hashFunc := range bf.hashFuncs {
		hash := fnv.New64a()
		hash.Write([]byte(element))
		bitIndex := uint(hash.Sum64()) % bf.size
		if (bf.vector[bitIndex/8] & (1 << (bitIndex % 8))) == 0 {
			return false
		}
	}
	return true
}