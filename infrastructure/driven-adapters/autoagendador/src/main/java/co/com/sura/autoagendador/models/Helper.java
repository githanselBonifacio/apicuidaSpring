package co.com.sura.autoagendador.models;

import org.javatuples.Pair;

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.IntStream;

public class Helper {
     private  Helper() {
            throw new IllegalStateException("Utility class");

    }

    public static Individuo intercambiarElementos(
            List<List<CitaGenetic>> citasGen, Pair<Integer,Integer> pos1, Pair<Integer,Integer> pos2) {

            List<List<CitaGenetic>> nuevaListaIndividuo = new ArrayList<>(citasGen);

            var val1 = nuevaListaIndividuo.get(pos1.getValue0()).get(pos1.getValue1());
            var val2 = nuevaListaIndividuo.get(pos2.getValue0()).get(pos2.getValue1());

            List<List<CitaGenetic>> nuevaListaIndividuoCopy = new ArrayList<>(nuevaListaIndividuo);
            nuevaListaIndividuoCopy.get(pos1.getValue0()).set(pos1.getValue1(), val2);
            nuevaListaIndividuoCopy.get(pos2.getValue0()).remove((int)pos2.getValue1());
            nuevaListaIndividuoCopy.get(pos2.getValue0()).add(val1);
            return new Individuo(nuevaListaIndividuoCopy);

    }

    public static Individuo cambiarElementos(
            List<List<CitaGenetic>> citasGen, Pair<Integer,Integer> pos1, Pair<Integer,Integer> pos2){

        List<List<CitaGenetic>> nuevaListaIndividuo = new ArrayList<>(citasGen);

        var val1 = nuevaListaIndividuo.get(pos1.getValue0()).get(pos1.getValue1());
        List<List<CitaGenetic>> nuevaListaIndividuoCopy = new ArrayList<>(nuevaListaIndividuo);
        nuevaListaIndividuoCopy.get(pos1.getValue0()).remove((int) pos1.getValue1());
        nuevaListaIndividuoCopy.get(pos2.getValue0()).add(val1);
        return new Individuo(nuevaListaIndividuoCopy);

    }
    public static int maxSumaLista(List<List<Integer>> lista) {

        return  IntStream.range(0, lista.size())
                .boxed()
                .max(Comparator.comparingInt(i -> lista.get(i).stream().mapToInt(Integer::intValue).sum()))
                .orElse(-1);
    }


    public static List<Integer> encontrarSumandos(int numero, int numerosSumandos) {
        List<Integer> sumandos = new ArrayList<>();
        var suma = 0;
        var random = new SecureRandom();

        for (var i = 0; i < numerosSumandos; i++) {
            if (i < numerosSumandos - 1) {
                int limit = numero - numerosSumandos - suma;
                if (limit <= 0) {
                    limit = 1;
                }
                int counter = random.nextInt(limit) + 1;
                sumandos.add(counter);
                suma += counter;
            } else {
                int counter = numero - suma;
                sumandos.add(counter);
                suma += counter;
            }
        }

        return sumandos;
    }
}
