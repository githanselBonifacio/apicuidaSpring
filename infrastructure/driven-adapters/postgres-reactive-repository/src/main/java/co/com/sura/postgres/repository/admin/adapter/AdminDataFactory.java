package co.com.sura.postgres.repository.admin.adapter;

import co.com.sura.entity.admin.ItemDiaTurno;
import co.com.sura.entity.admin.SecuenciaTurno;
import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.postgres.repository.admin.data.ItemSecuenciaTurnoData;


public interface AdminDataFactory {
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
