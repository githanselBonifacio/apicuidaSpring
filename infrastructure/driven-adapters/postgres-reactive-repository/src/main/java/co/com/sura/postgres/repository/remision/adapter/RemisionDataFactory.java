package co.com.sura.postgres.repository.remision.adapter;

import co.com.sura.entity.remision.ItemDiaTurno;
import co.com.sura.entity.remision.SecuenciaTurno;
import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.postgres.repository.personal.data.ItemSecuenciaTurnoData;


public interface RemisionDataFactory {
    static ItemSecuenciaTurnoData crearItemDiaTurnoData(
            SecuenciaTurno secuenciaTurno,ItemDiaTurno itemDiaTurno,HorarioTurno horarioTurno){
        return  ItemSecuenciaTurnoData
                .builder()
                .nombreSecuencia(secuenciaTurno.getNombre())
                .descripcion(secuenciaTurno.getDescripcion())
                .numeroDia(itemDiaTurno.getNumeroDia())
                .nombreDia(itemDiaTurno.getNombreDia())
                .idHorarioTurno(horarioTurno.getId())
                .build();
    }
}
