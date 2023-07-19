package co.com.sura.autoagendar;

import org.javatuples.Pair;


import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.stream.IntStream;

public class Helper {
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

    public static Integer calcularTiempoDesplazamiento(Double lat1,Double lon1,Double lat2, Double lon2 ){
        double earthRadius = 6371; // in kilometers
        double dLat = Math.toRadians(lat2 - lat1);
        double dLon = Math.toRadians(lon2 - lon1);
        double a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
                Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) *
                        Math.sin(dLon / 2) * Math.sin(dLon / 2);
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
        double distance = earthRadius * c;
        double speed = 50; // in kilometers per hour
        double time = distance / speed; // in hours
        //System.out.println(time*3600);
        return (int) (time*3600); // in minutes
    }

    public static List<Integer> encontrarSumandos(int numero, int numerosSumandos) {
        List<Integer> sumandos = new ArrayList<>();
        var suma = 0;
        var random = new Random();

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
