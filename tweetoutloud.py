#!/usr/bin/env python3
import twitter
from espeak.espeak import synth, is_playing
from time import sleep

def get_connection():
    import credentials as creds
    auth = twitter.OAuth(creds.TOKEN, creds.TOKEN_SECRET, 
                         creds.CONSUMER_KEY, creds.CONSUMER_SECRET)
    t = twitter.Twitter(auth = auth)
    return t

def clean_text(text, entities):
    replacements = []
    for hashtag in entities['hashtags']:
        start_idx, end_idx = hashtag["indices"]
        subs = "hash-tag "+hashtag["text"]
        replacements.append((text[start_idx:end_idx], subs))
    for user_mention in entities['user_mentions']:
        start_idx, end_idx = user_mention["indices"]
        subs = "At "+user_mention["name"]
        replacements.append((text[start_idx:end_idx], subs))
    for url in entities['urls']:
        start_idx, end_idx = url["indices"]
        subs = "LINK"
        replacements.append((text[start_idx:end_idx], subs))
    for from_ , to in replacements:
        text = text.replace(from_, to)
    return text

def talk(text):
    synth(text)
    while is_playing():
        sleep(.01)
    sleep(0.5)

def main():
    conn = get_connection()
    tweets = conn.statuses.home_timeline()
    for tweet in tweets:
        author = tweet['user']['name']
        text = tweet['text']
        cleaned = author+': '+clean_text(text, tweet['entities'])
        print(cleaned)
        talk(cleaned)

if __name__ == "__main__":
    main()
