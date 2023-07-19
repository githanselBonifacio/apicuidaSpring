package co.com.sura.entity.agenda;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Actividad implements Comparable<Actividad>{
    private String responsable;
    private String numeroIdentificacion;
    private String idMovil;
    private List<Tarea> tareas;

    @Override
    public int compareTo(Actividad a) {
        return a.tareas.size()-this.tareas.size();
    }
}
