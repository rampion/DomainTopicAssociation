#!/usr/bin/env ruby
require 'rubygems'
require 'net/http'
require 'nokogiri'

Signal.trap("INT") { exit }

ROOT = "http://www.alexa.com"
SITES =  'span.topsites-label'
SUBS = [
  'div#topsites-category > div.categories > div > ul > li > a',
  'div#topsites-category > div#catList > div.categories > ul > li > a'
].join(', ')
PAGES = 'a.pagination-page'

Connection = Net::HTTP.new 'www.alexa.com'

def fetch path
  Nokogiri::HTML(Connection.get(path).body.force_encoding('UTF-8'))
end
def search cat
  STDERR.print cat
  puts cat

  doc = fetch cat
  count = doc.css(PAGES).map do |a| 
    fetch a[:href]
  end.unshift(doc).flat_map do |page|
    page.css(SITES).map { |span| span.text }
  end.each_with_index do |url, index|
    puts "#{index}. #{url}"
  end.length
  STDERR.puts "\t#{count}"

  doc.css(SUBS).each { |a| search a[:href] }
end

search(ARGV[0] || "/topsites/category")
