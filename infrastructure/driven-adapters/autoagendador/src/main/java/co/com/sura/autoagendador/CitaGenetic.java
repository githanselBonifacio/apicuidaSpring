package co.com.sura.autoagendador;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static co.com.sura.autoagendador.Helper.encontrarSumandos;


@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class CitaGenetic implements Comparable<CitaGenetic>, Cloneable{
    private String idCita;
    private double latitud;
    private double longitud;
    private Integer duracion;
    private Integer holgura;
    private long fechaInicioIso;

    public static List<List<CitaGenetic>>dividirListaCitas(
            List<CitaGenetic> citas, Integer numeroDiviciones, CitaGenetic origen){

        List<Integer> sizes = encontrarSumandos(citas.size(), numeroDiviciones);
        Collections.shuffle(citas);
        if (sizes.stream().mapToInt(Integer::intValue).sum() != citas.size()) {
            throw new IllegalArgumentException("Sum of sizes must equal length of list.");
        }
        List<List<CitaGenetic>> listasAgrupadas = new ArrayList<>();
        var start = 0;
        for (int size : sizes) {
            int end = start + size;
            listasAgrupadas.add(citas.subList(start, end));
            start = end;
        }

        List<List<CitaGenetic>> listasAgrupadaOrigen = new ArrayList<>();
        for (List<CitaGenetic> citaGenetics : listasAgrupadas) {
            List<CitaGenetic> nuevaListaCitas = new ArrayList<>(citaGenetics);
            nuevaListaCitas.add(origen);
            nuevaListaCitas.sort(CitaGenetic::compareTo);
            listasAgrupadaOrigen.add(nuevaListaCitas);
        }

        return listasAgrupadaOrigen;

    }

    @Override
    public String toString() {
        return "("+ idCita + ")";
    }

    @Override
    public int compareTo(CitaGenetic citaGenetic) {
        return Long.compare(this.fechaInicioIso, citaGenetic.fechaInicioIso);
    }

    @Override
    protected CitaGenetic clone() throws CloneNotSupportedException {
        try {
            return (CitaGenetic) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }
}
