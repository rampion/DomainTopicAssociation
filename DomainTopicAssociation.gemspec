Gem::Specification.new do |s|
  s.name    = "DomainTopicAssociation"
  s.version = "1.0.0"

  s.summary = "Use the Open Directory to provide topics for Alexa's top 1M domains."
  s.author  = "Noah Luck Easterly"
  s.email   = "noah.easterly@gmail.com"
  s.homepage = "https://github.com/rampion/DomainTopicAssociation"
  s.description = File.read('README.md')

  s.has_rdoc  = false
  s.files     = Dir['lib/*', 'bin/*']
  s.executables.concat( Dir['bin/*'].map { |x| x.sub('bin/','') } )

  s.add_dependency 'ox'
  s.add_dependency 'addressable'
  s.add_dependency 'ruby-progressbar'
end
