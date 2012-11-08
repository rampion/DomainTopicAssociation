require 'rubygems'
require 'addressable/uri'


class PathTrie
  TrieNode = Struct.new( :url, :value, :children )
  attr_reader :size
  def PathTrie.get_keys(url)
    uri = Addressable::URI.parse url
    uri.host.split('.').reverse + uri.path.scan(%r!/[^/]*!)
  end
  def PathTrie.create_node
    TrieNode.new( nil, nil, Hash.new )
  end
  def initialize
    @trie = PathTrie.create_node
    @size = 0
  end
  def inspect
    "#<PathTrie @size=#@size @trie=...>"
  end
  def add_url url
    @size += 1
    leaf = PathTrie.get_keys(url).inject(@trie) do |root, key| 
      root.children[key] ||= PathTrie.create_node
    end
    leaf.url = url
    leaf.value = TopicTrie.new
  end
  def add_topic(url, topic)
    keys = PathTrie.get_keys(url)
    keys.each_with_index.inject(@trie)do |root, (key, index)|
      root.value.add_topic(topic, key[index .. -1]) if root.value
      root.children[key] or return
    end
  end
  include Enumerable
  def each(&blk)
    PathTrie.recurse_each(@trie, &blk)
  end
  def PathTrie.recurse_each(root, &blk)
    blk[root.url, root.value] if root.value
    root.children.values.each { |node| PathTrie.recurse_each(node, &blk) }
  end
end

class TopicTrie
  TrieNode = Struct.new( :value, :children, :count )
  def TopicTrie.get_keys(topic)
    topic.split('/')
  end
  def TopicTrie.create_node
    TrieNode.new( [], Hash.new { |_h,_k| _h[_k] = TopicTrie.create_node }, 0 )
  end
  def inspect
    "#<TopicTrie size=#{@trie.count} @trie=...>"
  end
  def initialize
    @trie = TopicTrie.create_node
  end
  def add_topic(topic, remainder)
    leaf = TopicTrie.get_keys(topic).inject(@trie) do |root, key| 
      root.count += 1
      root.children[key] 
    end
    leaf.count += 1
    leaf.value << remainder
    # how to determine scores for each topic
    # - function of length of remainder?, distance down trie?
  end
end

