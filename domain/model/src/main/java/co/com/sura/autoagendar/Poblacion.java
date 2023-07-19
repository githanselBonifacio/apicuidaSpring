package co.com.sura.autoagendar;

import lombok.AllArgsConstructor;
import lombok.Data;
import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
public class Poblacion implements Cloneable{
    private List<Individuo> individuos;

    public Poblacion() {
        this.individuos = new ArrayList<>();
    }

    @Override
    protected Poblacion clone()  {
        Poblacion cloned = null;
        try {
            cloned = (Poblacion) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
        cloned.individuos = new ArrayList<>(this.individuos);
        return cloned;
    }
}
