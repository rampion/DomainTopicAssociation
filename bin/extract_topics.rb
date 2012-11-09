#!/usr/bin/env ruby
# only tested with ruby-1.9 so far - you may have issues w/o that

require 'csv'
require 'sax_callbacks'
require 'file_status'
require 'Trie'

ALEXA_DOMAINS_CSV = 'data/top-1m.csv'
DMOZ_TOPICS_RDF   = 'data/content.rdf.u8'

unless File.readable? ALEXA_DOMAINS_CSV
  STDERR.puts "Please download #{ALEXA_DOMAINS_CSV} from http://s3.amazonaws.com/alexa-static/top-1m.csv.zip"
  exit -1
end
unless File.readable? DMOZ_TOPICS_RDF
  STDERR.puts "Please download #{DMOZ_TOPICS_RDF} from http://rdf.dmoz.org/rdf/content.rdf.u8.gz"
  exit -1
end

$domain_topics = PathTrie.new
CSV.open("top-1m.csv") do |csv|
  status("Loading top domains", file: csv) do |update|
    csv.each do |row|
      $domain_topics.add_url( "http://" + row[1] )
      update[]
    end
  end
end
STDERR.puts "#{$domain_topics.size} domains loaded"

File.open("content.rdf.u8", "r:UTF-8")  do |file|
  status("Loading topics", file: file) do |update|
    SaxCallbacks.parse( file ) do |url, topic|
      $domain_topics.add_topic( url, topic ) if @url
    end
  end
end

CSV.open("domain-topics.csv", "w:UTF-8") do |csv|
  status("Dumping topics", total: $domain_topics.length) do |update|
    $domain_topics.each do |domain, topics|
      csv << [domain, *topics.to_a]
      update[]
    end
  end
end
