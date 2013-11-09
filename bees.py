import random

def log(s):
    # pass
    print s

def minimize(f):
    """
    Return a positive number that varies inversely with the function given.
    """
    def fitness(x):
        fx = f(x)
        if fx >= 0:
            return 1.0 / (fx + 1)
        else:
            return 1.0 + abs(fx)
    return fitness


class Vector:
    size = 10
    
    def __init__(self, v):
        self.v = v
        self.clamp()
        
    @staticmethod
    def new():
        v = []
        for i in xrange(Vector.size):
            v.append(random.random() * 10)
        return Vector(v)
    
    @minimize
    def fitness(self):
        mean = sum(self.v) / float(len(self.v))
        total_squared_error = 0
        for elt in self.v:
            total_squared_error += (elt - mean) ** 2
        return total_squared_error / len(self.v)

    def clamp(self):
        self.v = [max(0, min(10, elt)) for elt in self.v]

    def __iter__(self):
        return iter(self.v)
        
    @staticmethod
    def mutate(vectors, n, alpha):
        """
        contruct a mutant v' from v_n as follows:
        for each element i in v_n:
        select a random v_m such that v_m != v_n
            v'[i] = v_n[i] + phi * (v_n[i] - v_m[i])
            (where phi is a random number between -alpha and alpha)
        In other words, tweak each value in v_n either towards or away from v_m.
        """
        def phi():
            return random.random() * 2 * alpha - alpha
        vector = vectors[n]
        mutant_vector = []
        for i, elt in enumerate(vector):
            m = random.randrange(len(vectors) - 1)
            if m >= n: m += 1
            diff = elt - vectors[m].v[i]
            mutant_vector.append(elt + phi() * diff)
        return Vector(mutant_vector)

    def __str__(self):
        return '[ ' + ', '.join(("%5.2f" % e for e in self.v)) + ' ]'

    
class Solution:
    max_attempts = 10
    
    def __init__(self, vector, fitness, attempts):
        self.vector = vector
        self.fitness = fitness
        self.attempts = attempts

    @staticmethod
    def from_vector(vector):
        fitness = vector.fitness()
        attempts = Solution.max_attempts
        return Solution(vector, fitness, attempts)

    @staticmethod
    def new():
        vector = Vector.new()
        fitness = vector.fitness()
        attempts = Solution.max_attempts
        return Solution(vector, fitness, attempts)

    def __str__(self):
        return "%8.3f %3d %s" % (self.fitness, self.attempts, str(self.vector))
    
class Hive:
    def __init__(self, workers=10, observers=10, alpha=1.0):
        self.solutions = []
        for i in xrange(workers):
            self.solutions.append(Solution.new())
        self.best = max(self.solutions, key=lambda s: s.fitness)
        self.alpha = alpha
        self.observers = observers
        
    def mutate(self, n):
        vectors = [s.vector for s in self.solutions]
        mutant_vector = Vector.mutate(vectors, n, self.alpha)
        return Solution.from_vector(mutant_vector)
        
    def work_on(self, n):
        current = self.solutions[n]
        mutant = self.mutate(n)
        if mutant.fitness > current.fitness:
            self.solutions[n] = mutant
            bang = '!'
            if mutant.fitness > self.best.fitness:
                self.best = mutant
                bang = '^'
        else:
            current.attempts -= 1
            bang = ' '
        log("work on %3d %s (%2d)" % (n, bang, self.solutions[n].attempts))
            
    def worker_phase(self):
        for n in xrange(len(self.solutions)):
            self.work_on(n)
            
    def observer_phase(self):
        fitnesses = [s.fitness for s in self.solutions]
        total_fitness = sum(fitnesses)
        for o in xrange(self.observers):
            i = 0
            r = random.random() * total_fitness
            while fitnesses[i] < r:
                r -= fitnesses[i]
                i += 1
            self.work_on(i)
                
    def scout_phase(self):
        for i, solution in enumerate(self.solutions):
            if solution.attempts <= 0:
                log("reset %2d" % i)
                self.solutions[i] = Solution.new()
        
    def __str__(self):
        lines = []
        for i, s in enumerate(self.solutions):
            lines.append("%4d  %s" % (i, str(s)))
        lines.append("BEST  " + str(self.best))
        return '\n'.join(lines)
        
    def iteration(self):
        self.worker_phase()
        self.observer_phase()
        self.scout_phase()
        log(str(self))

    def run(self, n):
        for i in xrange(n):
            self.iteration()
        return self.best

if __name__ == '__main__':
    hive = Hive()
    hive.run(1000)
