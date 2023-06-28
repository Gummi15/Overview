# This is a short description of the experience that I have in Python.

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# I have used Python for:
# - Data processing & data analysis: Regressions (linear, Lasso, Ridge), principal component analysis
# - Machine learning/Statistical leanrning: Neural Networks (fully connected, autoencoder, convolutional), natural language processing
# - Decision trees
# - Webscrapping

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Books I have read:

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Short overview of some of the packages I have experience with

# - Pandas --- https://pandas.pydata.org/
# - Matplotlip --- https://matplotlib.org/
# - Numpy --- https://numpy.org/
# - Selenium --- https://selenium-python.readthedocs.io/
# - Sklearn --- https://scikit-learn.org/stable/
# - Tensorflow ---https://www.tensorflow.org/
# - PyTorch --- https://pytorch.org/

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# The code presented here is from a project that I have worked on in my spare time and from some exercises from univeristy

# Code from spare time project --------------------------------------------------------------------------------------------------------------------------------------------------

class Underkategori():
    def __init__(self, name):
        self.name = name

    def fetch_products(self):
        driver.find_element(By.XPATH,f"/html/body/div[2]/main/div/div/div[3]/div[3]/div/div/div[2]/div/div/div[2]/div[2]//*[contains(translate(text(), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '{self.name.lower()}')]/parent::*//a").click()
        Produkter = driver.find_elements(By.CLASS_NAME, "product-wrap")
        lProduktnavn = []
        lVareinfo = []
        lAllNutinfo = []
        for i in range(len(Produkter)):
            time.sleep(0.5)

            try_max = 1
            try_count = 0

            while try_count < try_max:
                try:
                    Produkter[i].click()
                    break
                except Exception as e:
                    try_count += 1
                    
            time.sleep(0.5)
            lProduktnavn.append(driver.find_element(By.XPATH, "//main/div[2]/div/div/div/div/div[1]/div[2]//div[@class = 'title']").text)
            lVareinfo.append(driver.find_element(By.XPATH, "//div[contains(text(),'Varenummer:')]").text)
            nutInfo = driver.find_elements(By.CLASS_NAME, "nut-line")
            lNutinfo = []
            for j in range(len(nutInfo)):
                lNutinfo.append(nutInfo[j].text)
                #print(j)
            time.sleep(0.5)
            lAllNutinfo.append(lNutinfo)
            time.sleep(0.5)
            driver.find_element(By.XPATH, "/html/body/div[2]/main/div[2]/div/div/div/div/a").click()

        # pdAllInfo = pd.DataFrame(lAllNutinfo)
        pd1 = pd.DataFrame(lAllNutinfo)
        pd2 = pd.DataFrame(lVareinfo)
        pd3 = pd.DataFrame(lProduktnavn)
        pdAllInfo = pd.concat([pd3, pd2, pd1], axis=1)
        pdAllInfo.columns = ["Produktnavn", "Varenummer", "Næringsindhold", "kcal", "Fedt", "Heraf mættede fedtsyrer", "Kulhydrat", "Heraf sukkerarter", "Kostfibre", "Protein", "Salt"]

        self.pdAllInfo = pdAllInfo

def text_cleaner(dfInput):
    pdKJ = pd.DataFrame()
    pdKJ = []
    for j in range(len(dfInput["kcal"])):
        # print(j)
        tmpstr = dfInput["kcal"][j].split(" / ")
        # pdAllInfo["kcal"][j] = tmpstr[1].replace(" kcal", "")
        pdKJ.append(tmpstr[0])

    pdAllInfo["KJ"] = pdKJ

    for i in range(1, dfInput.shape[1]):
        print(i)
        dfInput.iloc[:, i] = dfInput.iloc[:, i].apply(lambda x: re.sub(r"\D+", "", x))

def reloader():
    try:
        driver.find_element(By.XPATH, "/html/body/div[2]/main/div[2]/div/div/div/div/a").click()
    except Exception as e:
        print(f"Error is {e}")
    driver.execute_script("window.history.go(-1)")

def translate_text(text, src_lang, dest_lang):
    translator = Translator(from_lang=src_lang, to_lang=dest_lang)
    translated = translator.translate(text)
    return translated

def find_most_similar_word(target_word, word_list):
    nlp = spacy.load("en_core_web_lg")
    target_vector = nlp(target_word).vector

    most_similar_word = None
    max_similarity = -1
    position = 0

    for word in word_list:

        if " " in word:
            #print("there is space")
            word = re.sub(r"[^a-zA-Z\s]", "", word)
            word = word.split(" ")
            word = [entry for entry in word if len(entry) != 0]
            word = [entry for entry in word if "and" not in entry]
        else:
            word = [word]

        for j in range(len(word)):
            word_vector = nlp(word[j]).vector
            similarity = target_vector.dot(word_vector)

            if similarity > max_similarity:
                most_similar_word = word[j]
                max_similarity = similarity
                Output_position = position
        position += 1

    tmp = {"target word" : [target_word], "most_similar_word": [most_similar_word], "position": [Output_position]}
    Output = pd.DataFrame(tmp)

    return Output


vMatches = find_most_similar_word(vAll_categories.iloc[0,0], vEnglish)
for j in range(1,vAll_categories.shape[0]):
    vMatches = pd.concat([vMatches, find_most_similar_word(vAll_categories.iloc[j,0], vEnglish)], ignore_index=True)

vIndeciestokeep = []
for j in vMatches["position"].unique():
    vTmpword = vMatches[vMatches["position"] == j]
    max_simlarity = 0
    if len(vTmpword["most_similar_word"].unique()) > 1:
        print("error")
    else:
        vTmptarget = vTmpword["most_similar_word"].unique()

    target_vector = nlp(vTmptarget[0]).vector

    iCount = 0
    for i in vTmpword["target word"]:
        similarity = target_vector.dot(nlp(i).vector)
        if similarity > max_simlarity:
            vIndextokeep = vTmpword["target word"].index[iCount]
        iCount =+ 1
    vIndeciestokeep.append(vIndextokeep)
    
vMatches = vMatches.iloc[vIndeciestokeep]

vProduktsToAdd = []
for i in range(len(vLinks)):
    word = vLinks[i].find_element(By.XPATH, "..").find_element(By.TAG_NAME, "span").text
    word = re.sub(r"[^a-zA-Z\s]", " ", word)
    word = word.split(" ")
    word = [entry for entry in word if len(entry) != 0]
    word = [entry for entry in word if "and" not in entry]
    word = pd.DataFrame(word)
    word = word.iloc[:, 0].apply(lambda x: translate_text(x, 'da', 'en').lower())

    vAnyWords = [j for j in (vtmpSubcategories['bread']) if j in (word)]

    if any(vAnyWords):
        vProduktsToAdd.append(i)

# Code from exercises --------------------------------------------------------------------------------------------------------------------------------------------------------

import torch
from torch import optim
from sklearn.datasets import load_boston
from sklearn import linear_model

dataset = load_boston()
X, y = dataset.data, dataset.target
X = (X - X.mean(axis=0))/(X.std(axis=0))
tX = torch.from_numpy(X).float()
ty = torch.from_numpy(y).float().view(-1, 1)

class NN():
    
    def __init__(self):
        pass

    def cost(self, X, y, W1, b1, W2, b2, c=0):
        loss = None
        hin = X @ W1 + b1
        hout = hin.clamp(min=0)
        pred = hout @ W2 + b2
        loss = torch.mean((pred-y)**2) 
        reg_loss = c * torch.sum(W1**2) + c * torch.sum(W2**2)
        loss = loss + reg_loss    
        return loss
    
    def fit(self, X, y, hidden_size=32, c=0.01):   

        input_dim = X.shape[1]        
        W1 = torch.randn(input_dim, hidden_size, requires_grad=True)
        b1 = torch.randn(1, hidden_size, requires_grad=True)
        W2 = torch.randn(hidden_size, 1, requires_grad=True)
        b2 = torch.randn(1, 1, requires_grad=True)
        
        sgd = optim.SGD(params={W1, W2, b1, b2}, lr=0.01)
        for i in range(100):
            sgd.zero_grad()
            loss = self.cost(X, y, W1, b1, W2, b2, c=c)
            if i % 10 == 0:
                print('epoch:', i, 'nn least squares loss', loss.item())
            loss.backward()
            sgd.step()
        
        self.W1 = W1
        self.W2 = W2
        self.b1 = b1
        self.b2 = b2
        
    def score(self, X, y):

        score = self.cost(X, y, self.W1, self.b1, self.W2, self.b2, c=0)
        return score


net = NN()
net.fit(tX, ty, hidden_size=16, c=0.01)
print('pytorch Neural Net Regression least squares score:', net.score(tX, ty).item())


from torchviz import make_dot # install this package 

x1 = torch.tensor([[3.]])
x2 = torch.tensor([[1.]])
y = torch.tensor([9.])
W1 = torch.tensor([[1.]], requires_grad=True)
W2 = torch.tensor([[2.]], requires_grad=True)
W3 = torch.tensor([[1.]], requires_grad=True)

nn_x = W3 * torch.clamp(W1 * x1 + W2*x2, min=0)
loss = (y - nn_x).pow(2)
loss.backward()

print('d_w1', W1.grad)
print('d_w2', W2.grad)
print('d_w3', W3.grad)
print('Lets show the computation graph')
make_dot(loss, params={'W1': W1, 'W2': W2, 'W3': W3})


import numpy as np
h_in = np.array([[-1, 2, 4]])
d_hout = np.array([[1,2,3]])
print('shapes:', h_in.shape, d_hout.shape)
def relu_grad(d_hout, hin):
    d_hin = None
    d_hin = d_hout.copy()
    d_hin[hin<0] = 0
    return d_hin

def sigmoid_grad(d_hout, hin):
    d_hin = None
    def sigmoid(x):
        return 1./(1+np.exp(-x))
    d_hin =  d_hout * (sigmoid(hin)*(1-sigmoid(hin))) # entrywise multiplication
    return d_hin

print('d_hin relu:', relu_grad(d_hout, h_in))
# should be [0, 2, 3]
print('d_hin sigmoid:', sigmoid_grad(d_hout, h_in))
# should be ~ [0.196..., 0.209..., 0.052...]


import torch
from torch import optim
from sklearn.datasets import load_boston
from sklearn import linear_model

print('*'*5, 'Load and Prepare Data', '*'*5)
dataset = load_boston()
# print('dataset', dataset)
X, y = dataset.data, dataset.target
X = (X - X.mean(axis=0))/(X.std(axis=0))
#print('data stats', X.shape, X.mean(axis=0), X.std(axis=0))
ridge=linear_model.Ridge(alpha=0.1, fit_intercept=True)
ridge.fit(X, y)
# print(ridge.coef_, ridge.intercept_)
print('\n', '*'*5, 'Test Sklearn Ridge Regression for Comparison', '*'*5)
print('Ridge Regression Score:', ((ridge.predict(X)-y)**2).mean())

print('\n', '*'*5, 'Make data to torch tensors', '*'*5)
tX = torch.from_numpy(X).float()
ty = torch.from_numpy(y).float().view(-1, 1)
