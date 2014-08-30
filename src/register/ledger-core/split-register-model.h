/********************************************************************\
 * split-register-model.h -- split register model object            *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef SPLIT_REGISTER_MODEL_H
#define SPLIT_REGISTER_MODEL_H

#include "table-model.h"

TableModel * gnc_split_register_model_new (void);
TableModel * gnc_template_register_model_new (void);

typedef enum
{
    /* Colors used for background drawing */
    COLOR_UNKNOWN_BG,
    COLOR_HEADER_BG,
    COLOR_PRIMARY_BG,
    COLOR_PRIMARY_BG_ACTIVE,
    COLOR_SECONDARY_BG,
    COLOR_SECONDARY_BG_ACTIVE,
    COLOR_SPLIT_BG,
    COLOR_SPLIT_BG_ACTIVE,

    /* Colors used for foreground drawing (text etc)
     * ATTENTION: the background and foreground lists should have
     *            the same types (the same amount of entries) !
     *            The code relies on this ! */
    COLOR_UNKNOWN_FG,
    COLOR_HEADER_FG,
    COLOR_PRIMARY_FG,
    COLOR_PRIMARY_FG_ACTIVE,
    COLOR_SECONDARY_FG,
    COLOR_SECONDARY_FG_ACTIVE,
    COLOR_SPLIT_FG,
    COLOR_SPLIT_FG_ACTIVE,

    /* Other colors */
    COLOR_NEGATIVE, /* Color to use for negative numbers */
} RegisterColor;

#endif
