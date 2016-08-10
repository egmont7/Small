#!env/bin/python
import click
import requests
import bs4
import json


def get_urls_on_list_page(n) -> list:
    url = 'http://beersmithrecipes.com/toprated/{}'.format(n)
    soup = bs4.BeautifulSoup(requests.get(url).text, 'html.parser')
    links = soup.find_all(title='View Recipe')
    return [link['href'] for link in links]


def get_recipe(url: str) -> dict:
    recipe = {}
    soup = bs4.BeautifulSoup(requests.get(url).text, 'html.parser')
    rows = soup.find(class_='recipes').find_all('tr')[1:]
    ingredients = []
    for row in rows:
        content = [td.text for td in row.find_all('td')]
        ingredients.append({'amount': content[0],
                            'name': content[1],
                            'type': content[2]})
    recipe['ingredients'] = ingredients
    soup = bs4.BeautifulSoup(requests.get(url).text, 'html.parser')
    rows = soup.find(class_='r_hdr').find_all('tr')
    for row in rows:
        for td in row.find_all('td'):
            try:
                key, value = [s.strip().lower() for s in td.text.split(':')]
                recipe[key] = value
            except ValueError:  # ill formatted td
                continue
    return recipe


def recipes_iter(num_recipes_requested: int):
    current_page = 1
    current_urls = get_urls_on_list_page(current_page)
    for _ in range(num_recipes_requested):
        if len(current_urls) == 0:
            current_page += 1
            current_urls = get_urls_on_list_page(current_page)
        yield get_recipe(current_urls.pop())


@click.group()
def cli():
    pass


@cli.command()
@click.option('--count', default=10, help='Number of recipes to download')
@click.option('--savefile', default='out.json',
              help='Where to save the downloaded json')
def download(count, savefile):
    click.echo('Downloading {} Beer recipes'.format(count))
    with open(savefile, 'w') as f:
        f.write('[')
        for i, recipe in enumerate(recipes_iter(count)):
            json.dump(recipe, f, indent='  ')
            if i < count-1:
                f.write(',\n')
        f.write(']')


@cli.command()
def train():
    click.echo('training...')


@cli.command()
def classify():
    click.echo('classifying...')

if __name__ == '__main__':
    cli()
