# Converter of timert format to tim format.
#
# Usage:
#   ruby convert.rb TIMERT_JSON_FILE_PATH > output.json

require 'json'

puts JSON.load(File.open(ARGV.first, 'r')).
  flat_map {|_, day| day['intervals']}.
  to_json
