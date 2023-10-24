package co.com.sura.entity.admin;

import co.com.sura.entity.maestro.HorarioTurno;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class ItemSecuenciaTurno {
    private Integer idSecuencia;
    private String nombreSecuencia;
    private String descripcion;
    private String nombreDia;
    private Integer numeroDia;
    private List<HorarioTurno> horariosTurno;
}
