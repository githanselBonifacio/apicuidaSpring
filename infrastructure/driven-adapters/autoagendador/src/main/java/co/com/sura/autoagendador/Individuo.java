package co.com.sura.autoagendador;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Individuo implements Comparable<Individuo>, Cloneable{
    private List<List<CitaGenetic>> citaGen;

    @Override
    public int compareTo(Individuo o) {
        return this.citaGen.get(0).get(0).compareTo(o.citaGen.get(0).get(0));

    }
    public void sortCitaGen() {
        for (List<CitaGenetic> sublist : citaGen) {
            sublist.sort(CitaGenetic::compareTo);
        }
    }

    @Override
    protected Individuo clone() {
        try {
            Individuo cloned = (Individuo) super.clone();
            List<List<CitaGenetic>> clonedCitaGen = new ArrayList<>();
            for (List<CitaGenetic> citas : citaGen) {
                List<CitaGenetic> clonedCitas = new ArrayList<>();
                for (CitaGenetic cita : citas) {
                    clonedCitas.add(cita.clone());
                }
                clonedCitaGen.add(clonedCitas);
            }
            cloned.citaGen = clonedCitaGen;
            return cloned;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }
}
