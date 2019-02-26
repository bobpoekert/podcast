from hashlib import sha1, md5
from base64 import b64encode
from urllib import unquote

url = '''https://client-api.itunes.apple.com/WebObjects/MZStorePlatform.woa/wa/lookup?id=1445054297%2C1447288845%2C1447892344%2C1449500734%2C1450215523%2C1450277129%2C1451109634%2C1452065072
p=item
caller=DI6
requestParameters=%5Bobject%20Object%5D
version=1
X-JS-SP-TOKEN=VmaZRo6KcXASi67YNZFmWA%3D%3D
X-JS-TIMESTAMP=1551147682'''

Ids = '1445054297,1447288845,1447892344,1449500734,1450215523,1450277129,1451109634,1452065072'
caller = 'DI6'
p = 'item'
timestamp = '1551147682'
storefront = '143441-1,32'
dsid = 'null'

target = 'VmaZRo6KcXASi67YNZFmWA=='

sig_string = timestamp + storefront + caller + ids + p
sig = b64encode(md5(sig_string).digest())

print repr(sig), target, sig == target
print sig_string

