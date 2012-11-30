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
def search cat, filter
  STDERR.print cat

  doc = fetch cat

  count = unless filter
    puts cat
    doc.css(PAGES).map do |a| 
      fetch a[:href]
    end.unshift(doc).flat_map do |page|
      page.css(SITES).map { |span| span.text }
    end.each_with_index do |url, index|
      puts "#{index}. #{url}"
    end.length
  end

  STDERR.puts "\t#{count}"

  doc.css(SUBS).each do |a| 
    sub = a[:href]

    unless filter
      search( sub, false )
    else
      next unless RESTART_AT.start_with? sub
      case RESTART_AT[sub.length]
      when nil
        search( sub, false )
      when '/'
        search( sub, true )
      else 
        next
      end
      filter = false
    end
  end
end


RESTART_AT = ARGV[0]
search("/topsites/category", RESTART_AT )
