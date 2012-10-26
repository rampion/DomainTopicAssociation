#!/usr/bin/env ruby
# only tested with ruby-1.9 so far - you may have issues w/o that

=begin
INPUT
  top-1m.csv      - from http://s3.amazonaws.com/alexa-static/top-1m.csv.zip
                  - list of "Top Sites" from http://www.alexa.com/topsites
                    mainly seems to be domains, but there are a few domain+paths in there
  content.rdf.u8  - from http://rdf.dmoz.org/rdf/content.rdf.u8.gz
                    Open Directory list of topic/urls from http://rdf.dmoz.org/
OUTPUT
  domain-topics.csv

For the domains given in top-1m.csv, it locates all the topics affiliated for that 
domain in content.rdf.u8 (using a SAX parser) and dumps those affiliations into a csv file.

For example, if top-1m.csv listed toonhound.com, and content.rdf.u8 contained:
  <ExternalPage about="http://www.toonhound.com/">
    <d:Title>Toonhound</d:Title>
    <d:Description>British cartoon, animation and comic strip creations - links, reviews and news from the UK.</d:Description>
    <topic>Top/Arts/Animation</topic>
  </ExternalPage>
then domain-topics.csv could contain
  toonhound.com,Top/Arts/Animation
=end

unless File.readable? 'top-1m.csv'
  STDERR.puts "Please download top-1m.csv from http://s3.amazonaws.com/alexa-static/top-1m.csv.zip"
  exit -1
end
unless File.readable? 'content.rdf.u8'
  STDERR.puts "Please download content.rdf.u8 from http://rdf.dmoz.org/rdf/content.rdf.u8.gz"
  exit -1
end

require 'set'
require 'csv'

require 'rubygems'
require 'nokogiri'
require 'inline'

CHARS = %W{ - \\ | / }
SCALE = 1

# simple status display
def status(prefix=nil)
  if prefix
    $prefix = prefix
    $count = 0
    STDERR.puts
  else
    $count += 1
    return unless $count % SCALE == 0
  end
  STDERR.print "#$prefix #{CHARS[ ($count / SCALE) % 4 ]}\r"
  STDERR.flush
end

# Callbacks for the SAX parser
class NXSDoc < Nokogiri::XML::SAX::Document
  def initialize
    @path = []
    @topics = nil
  end
  def start_element(element, attributes)
    @path.push element
    case @path
    when ['RDF', 'ExternalPage']
      status
      
      domain = Hash[attributes]['about'].sub(%r!^\w+://([^"/]*)(?:/[^"]*)?$!, '\1')
      until @topics = $domain_topics[domain]
        domain.sub!(/^[^.]+\./,'') or break
      end
    when ['RDF', 'ExternalPage', 'topic' ]
      @topic = "" if @topics
    end
  end
  def characters(content)
    @topic << content if @topic
  end
  def getEntity
    'foo'
  end
  def end_element(element)
    case @path
    when ['RDF', 'ExternalPage']
      @topics = nil
    when ['RDF', 'ExternalPage', 'topic' ]
      @topics << @topic if @topics
      @topic = nil
    end
    @path.pop
  end
  def error(string)
    true
  end
end

# HACK: 
#   So the XML that OpenDirectory generates may contain some invalid
#   XML entities (like &#8;) that will cause libxml-based SAX parsers 
#   to flame and die unless we run it in recovery mode.
#
#   However, none of the ruby wrappers for libxml (nokogiri, libxml-ruby)
#   have a way to enable recovery mode.  So I'm using Inline::C to hack
#   one in.
#
#   To make this hack work, I need to know where libxml2's headers are installed.
#   I can't think of a clever way to do this right now, sow they're hardcoded
#   to require an environment variable.
LIBXML2_HEADERS = ENV["LIBXML2_HEADERS"]
unless LIBXML2_HEADERS and File.directory? LIBXML2_HEADERS
  STDERR.puts "Unable to locate libxml2 headers, try setting them manually using the LIBXML2_HEADERS environment variable."
  exit -1
end
class Nokogiri::XML::SAX::ParserContext
  inline do |builder|
    builder.add_compile_flags("-I" + LIBXML2_HEADERS)
    builder.include "<libxml/parser.h>"
    builder.struct_name = 'xmlParserCtxt'
    builder.accessor :recovery, 'int'
  end
end

status "Loading top domains..."
$domain_topics = Hash.new 
CSV.foreach("top-1m.csv") do |row|
  $domain_topics[row[1]] = Set.new
  status
end
STDERR.puts "\n#{$domain_topics.length} domains loaded"

status "Loading topics..."
Nokogiri::XML::SAX::Parser.new(NXSDoc.new).parse( File.open("content.rdf.u8", "r:UTF-8") ) do |ctxt|
  ctxt.recovery = 1 # turn recovery mode on
end

status "Dumping topics..."
CSV.open("domain-topics.csv", "w:UTF-8") do |csv|
  $domain_topics.each do |domain, topics|
    csv << [domain, *topics.to_a]
    status
  end
end

STDERR.puts
