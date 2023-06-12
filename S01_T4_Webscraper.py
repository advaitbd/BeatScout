from random import randint
import sys
import os
import numpy as np
import re
import json
import datetime
import selenium
import urllib
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import selenium
import urllib.request
import os
import shutil
import time
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException
import pandas as pd

def opentiktok(song_name):
    options = Options()
    options.add_argument('profile-directory=Profile 10')
    options.add_argument("user-data-dir=C:\\Users\\ngwee\\AppData\\Local\\Google\\Chrome\\AutomationProfile")
    options.page_load_strategy = 'normal'
    options.add_experimental_option('excludeSwitches', ['enable-logging'])
    global driver
    global skip_indicator

    try:
        if driver == None:
            driver = webdriver.Chrome(os.getcwd() + r'\chromedriver.exe', options=options)
    except:
        driver = webdriver.Chrome(os.getcwd() + r'\chromedriver.exe', options=options)

    try:
        skip_indicator = 0
        driver.get(
            'https://www.google.com/search?q=' + song_name + ' site:https://www.tiktok.com/music')
    except:
        skip_indicator = 1
        # x = input("CAPCHA")
    # while(True):
    #    pass

def input_search(song_name):
    driver.back()
    # x = input()
    option1 = WebDriverWait(driver, 5).until(
                EC.presence_of_element_located((By.XPATH,'//*[@id="tsf"]/div[1]/div[1]/div[2]/div/div[2]/input')))
    option1.clear()
    option1.send_keys(song_name + ' site:https://www.tiktok.com/music')
    option1.send_keys(Keys.RETURN)
    # while(True):
    #     pass



def click_options():
    option1 = WebDriverWait(driver, 5).until(
                EC.presence_of_element_located((By.XPATH,'//*[@id="quickmatch-aria-tabpanel"]/div/div/div[1]/div[1]/div[2]/div[2]/button')))
    #option1 = driver.find_element_by_css_selector('h3 LC20lb DKV0Md'.replace(' ','.'))
    driver.execute_script("arguments[0].click();", option1)

def click_google_option():

    try:
        skip_indicator = 0
        option1 = WebDriverWait(driver, 5).until(
                    EC.presence_of_element_located((By.CSS_SELECTOR,'h3 LC20lb DKV0Md'.replace(' ','.'))))
        #option1 = driver.find_element_by_css_selector('h3 LC20lb DKV0Md'.replace(' ','.'))
        driver.execute_script("arguments[0].click();", option1)
    except:
        skip_indicator = 1
        time.sleep(5)
        # x = input("CAPCHA")




def get_videos_created():
    try:
        option1 = WebDriverWait(driver, 3).until(
                        EC.presence_of_element_located((By.XPATH,'//*[@id="main-content-single_song"]/div/div/div[1]/div[2]/h2[2]/strong')))
        return(option1.text)
    except:
        return("0")




def main():
    df = pd.read_csv('Tiktok dataset.csv')

    # print(df.iloc[1,2])
    # opentiktok(df.iloc[1,2] +" "+ df.iloc[1,2])
    # input_search()
    # # print(i)
    # # print(df.iloc[i,2] +" "+ df.iloc[i,2])
    # click_google_option()

    # # df.iloc[i,1] = get_videos_created()
    # # df.to_csv(r"C:\Users\ngwee\OneDrive - Nanyang Technological University\Documents\NBS\BC2407 - ANALYTICS II ADV PRED TECH\Assignment\Final Folder\Tiktok dataset.csv")
    
    print(df.iloc[1,2])
    #Open tiktok with search url
    opentiktok(df.iloc[1,2] +" "+ df.iloc[1,2])
    #Click first google option
    click_google_option()
    #Get the count of videos created
    df.iloc[1,1] = get_videos_created()
    df.to_csv(r"C:\Users\ngwee\OneDrive - Nanyang Technological University\Documents\NBS\BC2407 - ANALYTICS II ADV PRED TECH\Assignment\Final Folder\Tiktok dataset.csv", index = False)
    

    skip_indicator = 0
    for i in range(119, 970):
        time.sleep(randint(0,5))

        print(df.iloc[i,2])
        #Open tiktok with search url
        print(i)
        print(df.iloc[i,2] +" "+ df.iloc[i,3])
        #Click first google option
        if(skip_indicator == 0):
            input_search(df.iloc[i,2] +" "+ df.iloc[i,3])
        else:
            opentiktok(df.iloc[i,2] +" "+ df.iloc[i,3])            
        click_google_option()
        #Get the count of videos created
        df.iloc[i,1] = get_videos_created()
        df.to_csv(r"C:\Users\ngwee\OneDrive - Nanyang Technological University\Documents\NBS\BC2407 - ANALYTICS II ADV PRED TECH\Assignment\Final Folder\Tiktok dataset.csv", index = False)

    


main()
