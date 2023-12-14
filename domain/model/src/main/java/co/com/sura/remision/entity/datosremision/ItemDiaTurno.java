package co.com.sura.remision.entity.datosremision;

import co.com.sura.maestros.entity.HorarioTurno;
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
public class ItemDiaTurno {
    private Integer numeroDia;
    private String nombreDia;
    private List<HorarioTurno> horariosTurno;

    public static ItemDiaTurno inicializarListaHorarioTurnoVacio(ItemDiaTurno itemDiaTurno){
        if(itemDiaTurno.getHorariosTurno().isEmpty()){
            var horarioTurnos = new ArrayList<HorarioTurno>();
            horarioTurnos.add(HorarioTurno.builder().build());
            itemDiaTurno.setHorariosTurno(horarioTurnos);
        }
        return itemDiaTurno;
    }
}
