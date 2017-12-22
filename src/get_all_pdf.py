'''
File: get_all_pdf.py
Author: Sidafa Conde
Email: sconde@umassd.edu
School: UMass Dartmouth
Date:  9/15/2017
Purpose: get the list of url for all the pdf files available on the server
'''
import mechanize
from bs4 import BeautifulSoup
# import os

main_link = "http://www.brocktonpolice.com/category/police-log/"
bp_wp_content = "http://www.brocktonpolice.com/wp-content/uploads"

def getYear():
    br = mechanize.Browser(factory=mechanize.RobustFactory())
    br.set_handle_robots(False)
    webpage = br.open(bp_wp_content).read()
    soupPage = BeautifulSoup(webpage, "html")
    soup = soupPage.find("ul").findAll('li')[1:]
    list_of_year = [x.string.encode('ascii', 'ignore').strip()[:-1] for x in
                    soup][:-2]
    good_year = map(lambda x: bp_wp_content + '/' + x, list_of_year)
    return good_year

def getMonths(list_of_year):
    list_to_return = []
    for item in list_of_year:
        br = mechanize.Browser(factory=mechanize.RobustFactory())
        br.set_handle_robots(False)
        webpage = br.open(item).read()
        soupPage = BeautifulSoup(webpage, "html").find('ul').findAll('li')[1:]
        months = [x.string.encode('ascii', 'ignore').strip()[:-1] for x in
                        soupPage][:-2]
        months = map(lambda x: item + '/' + x, months)
        for mth in months:
            list_to_return.append(mth)
    return list_to_return

def getListOfContent(months):
    list_to_return = []
    for mth in months:
        br = mechanize.Browser(factory=mechanize.RobustFactory())
        br.set_handle_robots(False)
        webpage = br.open(mth).read()
        soupPage = BeautifulSoup(webpage, "html").find('ul').findAll('li')[1:]
        cont = [x.find('a')['href'] for x in soupPage]
        cont = filter(lambda x: '.pdf' in x, cont)
        list_of_cont = [mth + '/' + x for x in cont]
        for x in list_of_cont:
            list_to_return.append(x)
    return list_to_return

if __name__ == "__main__":
    # first get the year
    year = getYear()
    months = getMonths(year)
    cont = filter(lambda x: len(x) > 0,getListOfContent(months))
    print (cont)


