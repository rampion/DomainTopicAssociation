#!/usr/bin/env ruby -I .
require 'rubygems'
require 'polyglot'
require 'treetop'
require 'alexa_scrape.tt'

Parser = AlexaScrapeParser.new
Doc = File.read("alexa_scrape500.txt", nil, nil, :encoding => "UTF-8")
Result = Parser.parse(Doc)

Result.category_listings.each do |cl|
  p cl
end

