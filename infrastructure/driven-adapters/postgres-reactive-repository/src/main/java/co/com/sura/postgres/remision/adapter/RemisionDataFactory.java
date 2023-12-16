package co.com.sura.postgres.remision.adapter;

import co.com.sura.postgres.personal.data.ItemSecuenciaTurnoData;
import co.com.sura.remision.entity.datosremision.ItemDiaTurno;
import co.com.sura.personal.entity.SecuenciaTurno;
import co.com.sura.maestros.entity.HorarioTurno;


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
