# Neural Network with one hidden layer using the backpropagation
# algorithm. Sigmoid activation function. For one output only (for now).
# Author: Gustavo (lgbpinho153@yahoo.com.br)
###############################################################################
start<-Sys.time()
### simple data
#Inputs<-rbind(rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1))
Inputs<-rbind(seq(1,20,0.1),seq(1,20,0.1),seq(1,20,0.1))
Targets<-10+2*Inputs[1,]+3*Inputs[2,]+5*Inputs[3,]

### preprocessing
maximum<-max(Targets);minimum<-min(Targets)
Targets<-(Targets-minimum)/(maximum-minimum)
for (i in 1:dim(Inputs)[1]){
    Inputs[i,]<-(Inputs[i,]-min(Inputs[i,]))/(max(Inputs[i,])-min(Inputs[i,]))
}

### Network parameters
NumInputs<-dim(Inputs)[1]
NumHidden<-2
NumOutputs<-1
WeightsHI<-matrix(rnorm(NumHidden*(NumInputs+1),0,1),NumHidden,NumInputs+1) # bias and weights between input and hidden layers
WeightsOH<-matrix(rnorm(NumOutputs*(NumHidden+1),0,1),NumOutputs,NumHidden+1) # bias and weights between hidden and output layers
SumHidden<-rep(0,NumHidden) # the induced field in each neuron
SumOutput<-rep(0,NumOutputs) # the induced field in each output neuron
DeltaH<-rep(0,NumHidden) # local gradient for hidden neurons
DeltaO<-rep(0,NumOutputs) # local gradient for output neurons
DeltaWHI<-matrix(0,NumHidden,NumInputs+1) # weight update matrix for the Hidden-Inputs weights
DeltaWOH<-matrix(0,NumOutputs,NumHidden+1) # weight update matrix for the Hidden-Inputs weights
Lr<-0.1 # learning rate
#Mf<-0.05 # momentum factor
Epochs<-5000
N<-length(Targets) # number of examples presented to the net
MSEplot<-rep(0,Epochs) # I'll plot the errors later

### Sigmoid Function
phi<-function(v){
    1/(1+exp(-v))
}

phidash<-function(v){
    phi(v)*(1-phi(v))
}

### Training
for (s in 1:Epochs){ # for each epoch
    MSE<-0 #mean squared error
    for (t in 1:N){ # for each example 
        ### Forward Pass
        HiddenInput<-c(1,Inputs[,t]) # inputs for the hidden layer
        SumHidden<-rep(0,NumHidden)
        for (i in 1:NumHidden){ # for each neuron 
            for (j in 1:NumInputs+1){ # for each input
                SumHidden[i]<-SumHidden[i]+WeightsHI[i,j]*HiddenInput[j]
            }
        }
        HiddenOut<-phi(SumHidden)
        OutputInput<-c(1,HiddenOut) # this is the input for the output layer
        SumOutput<-rep(0,NumOutputs)
        for (i in 1:NumOutputs){ # for each neuron 
            for (j in 1:NumHidden+1){ # for each input
                SumOutput[i]<-SumOutput[i]+WeightsOH[i,j]*OutputInput[j]
            }
        }
        Outputs<-phi(SumOutput)
        Error<-Targets[t]-Outputs #corrigir isso para mais neuronios
        MSE<-MSE+0.5*Error^2
        ### Backpropagation
        for (i in 1:NumOutputs){
            DeltaO[i]<-Error[i]*phidash(SumOutput[i])
        }
        for (i in 1:NumOutputs){  # for each output neuron
            for (j in 1:NumHidden+1){ # for each weight
                DeltaWOH[i,j]<-Lr*DeltaO[i]*OutputInput[j]
            }
        }
        DeltaH<-rep(0,NumHidden)
        for (i in 1:(NumHidden)){
            for (j in 1:NumOutputs){
                DeltaH[i]<-DeltaH[i]+DeltaO[j]*WeightsOH[j,i+1] # beware of teh bias
            }
            DeltaH[i]<-phidash(SumHidden[i])*DeltaH[i]
        }
        DeltaWHI<-matrix(0,NumHidden, NumInputs+1)
        for (i in 1:NumHidden){
            for (j in 1:NumInputs+1){
                DeltaWHI[i,j]<-Lr*DeltaH[i]*HiddenInput[j]
            }
        }
        WeightsOH<-WeightsOH+DeltaWOH
        WeightsHI<-WeightsHI+DeltaWHI
    }
    MSE<-MSE/N
    MSEplot[s]<-MSE
    if(s%%100==0) {cat("MSE for the last element at epoch ");cat(s);cat(" is: ");cat(MSE);cat("\n")}
    if (abs(Error)<1e-06) {cat(Error);cat("\n");break}
}
plot(MSEplot)
print(WeightsOH)
print(WeightsHI)
stop<-Sys.time()
time<-start-stop
cat("Tempo gasto em minutos: ");cat(stop-start);cat("\n")

### Simulation
OutputsSim<-rep(0,N)
for (t in 1:N){ # for each example 
    ### Forward Pass
    HiddenInput<-c(1,Inputs[,t]) # inputs for the hidden layer
    SumHidden<-rep(0,NumHidden)
    for (i in 1:NumHidden){ # for each neuron 
        for (j in 1:NumInputs+1){ # for each input
            SumHidden[i]<-SumHidden[i]+WeightsHI[i,j]*HiddenInput[j]
        }
    }
    HiddenOut<-phi(SumHidden)
    OutputInput<-c(1,HiddenOut) # this is the input for the output layer
    SumOutput<-rep(0,NumOutputs)
    for (i in 1:NumOutputs){ # for each neuron 
        for (j in 1:NumHidden+1){ # for each input
            SumOutput[i]<-SumOutput[i]+WeightsOH[i,j]*OutputInput[j]
        }
    }
    OutputsSim[t]<-phi(SumOutput)
}
ErrorsSim<-Targets-OutputsSim
#X11()
#plot(ErrorsSim)
cat(sum(ErrorsSim^2)/N);cat("\n")
