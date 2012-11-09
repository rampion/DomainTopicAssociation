#!/usr/bin/env ruby
require 'cgi'
require 'rubygems'
require 'ox'

class SaxCallbacks < ::Ox::Sax
  # uses simple state machine to see where we are relative to the root
  S_RDF           = 'RDF'
  S_ExternalPage  = 'ExternalPage'
  S_topic         = 'topic'
  S_about         = 'about'
 
  ROOT_START_ELEMENT = lambda do |name|
    set_state( S_RDF == name ? :rdf : :depth )
  end

  RDF_START_ELEMENT = lambda do |name|
    set_state( S_ExternalPage == name ? :externalpage : :depth )
  end
  RDF_END_ELEMENT = lambda do |name|
    set_state :root
  end

  EXTERNALPAGE_START_ELEMENT = lambda do |name|
    set_state( S_topic == name ? :topic : :depth )
  end
  EXTERNALPAGE_ATTR = lambda do |name, value|
    @url = CGI.unescape_html( value ) if S_about == name
  end
  EXTERNALPAGE_END_ELEMENT = lambda do |name|
    set_state :rdf
  end

  TOPIC_START_ELEMENT = lambda do |name|
    set_state :depth
  end
  TOPIC_TEXT = lambda do |value|
    @progress[@url, value]
  end
  TOPIC_END_ELEMENT = lambda do |name|
    set_state :externalpage
  end

  DEPTH_START_ELEMENT = lambda do |name|
    @depth += 1
  end
  DEPTH_END_ELEMENT = lambda do |name|
    if 0 == @depth
      set_state @parent
    else
      @depth -= 1
    end
  end

  def set_state state
    case state
    when :root
      @metaclass.send( :define_method, :start_element, ROOT_START_ELEMENT )
      @metaclass.send( :remove_method, :attr )
      @metaclass.send( :remove_method, :text )
      @metaclass.send( :remove_method, :end_element )
    when :rdf
      @url = nil
      @metaclass.send( :define_method, :start_element, RDF_START_ELEMENT )
      @metaclass.send( :remove_method, :attr )
      @metaclass.send( :remove_method, :text )
      @metaclass.send( :remove_method, :end_element, RDF_END_ELEMENT )
    when :externalpage
      @metaclass.send( :define_method, :start_element, EXTERNALPAGE_START_ELEMENT )
      @metaclass.send( :remove_method, :attr, EXTERNALPAGE_ATTR )
      @metaclass.send( :remove_method, :text )
      @metaclass.send( :remove_method, :end_element, EXTERNALPAGE_END_ELEMENT )
    when :topic
      @metaclass.send( :define_method, :start_element, TOPIC_START_ELEMENT )
      @metaclass.send( :remove_method, :attr )
      @metaclass.send( :remove_method, :text, TOPIC_TEXT )
      @metaclass.send( :remove_method, :end_element, TOPIC_END_ELEMENT )
    when :depth
      # context-sensitive state
      @depth = 0
      @parent = @state
      @metaclass.send( :define_method, :start_element, DEPTH_START_ELEMENT )
      @metaclass.send( :remove_method, :attr )
      @metaclass.send( :remove_method, :text )
      @metaclass.send( :define_method, :end_element, DEPTH_END_ELEMENT )
    end
    @state = state
  end

  def initialize(&progress)
    @progress = &progress
    @metaclass = class << self ; self ; end
    set_state :root
  end

  def self.parse(io, &progress)
    Ox.sax_parse( new(&progress), io )
  end
end
