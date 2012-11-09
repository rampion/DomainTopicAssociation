#!/usr/bin/env ruby
require 'rubygems'
require 'addressable/uri'
uri = Addressable::URI.parse('http://outer_heaven4.tripod.com/index2.htm') 
puts uri.host 
